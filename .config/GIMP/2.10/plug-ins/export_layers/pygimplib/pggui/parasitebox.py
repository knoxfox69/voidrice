# -*- coding: utf-8 -*-
#
# Copyright (C) 2014-2019 khalim19
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""
This module defines a custom widget acting as a GUI element for `gimp.Parasite`
instances.
"""

from __future__ import absolute_import, division, print_function, unicode_literals
from future.builtins import *

import pygtk
pygtk.require("2.0")
import gtk
import gobject

import gimp

from .. import pgconstants

__all__ = [
  "ParasiteBox",
]


class ParasiteBox(gtk.HBox):
  """
  This is a subclass of `gtk.VBox` to edit `gimp.Parasite` instances
  interactively.
  
  Signals:
  
  * `"parasite-changed"` - The parasite was modified by the user.
  """
  
  __gsignals__ = {b"parasite-changed": (gobject.SIGNAL_RUN_FIRST, None, ())}
  
  _HBOX_SPACING = 5
  _VBOX_SPACING = 3
  
  def __init__(self, parasite):
    gtk.HBox.__init__(self)
    
    self._should_invoke_parasite_changed_signal = True
    
    self._init_gui(parasite)
  
  def get_parasite(self):
    return gimp.Parasite(*self._get_values())
  
  def set_parasite(self, parasite):
    self._set_values(parasite)
  
  def _init_gui(self, parasite):
    self._parasite_name_entry = gtk.Entry()
    
    self._parasite_flags_spin_button = gtk.SpinButton(
      gtk.Adjustment(
        value=parasite.flags,
        lower=0,
        upper=2**32,
        step_incr=1,
        page_incr=10,
      ),
      digits=0)
    self._parasite_flags_spin_button.set_numeric(True)
    
    self._parasite_data_entry = gtk.Entry()
    
    self._vbox_name_label = gtk.Label(
      _("Name").encode(pgconstants.GTK_CHARACTER_ENCODING))
    self._vbox_name_label.set_alignment(0.0, 0.5)
    
    self._vbox_name = gtk.VBox()
    self._vbox_name.set_spacing(self._VBOX_SPACING)
    self._vbox_name.pack_start(self._vbox_name_label, expand=False, fill=False)
    self._vbox_name.pack_start(self._parasite_name_entry, expand=False, fill=False)
    
    self._vbox_flags_label = gtk.Label(
      _("Flags").encode(pgconstants.GTK_CHARACTER_ENCODING))
    self._vbox_flags_label.set_alignment(0.0, 0.5)
    
    self._vbox_flags = gtk.VBox()
    self._vbox_flags.set_spacing(self._VBOX_SPACING)
    self._vbox_flags.pack_start(self._vbox_flags_label, expand=False, fill=False)
    self._vbox_flags.pack_start(
      self._parasite_flags_spin_button, expand=False, fill=False)
    
    self._vbox_data_label = gtk.Label(
      _("Data").encode(pgconstants.GTK_CHARACTER_ENCODING))
    self._vbox_data_label.set_alignment(0.0, 0.5)
    
    self._vbox_data = gtk.VBox()
    self._vbox_data.set_spacing(self._VBOX_SPACING)
    self._vbox_data.pack_start(self._vbox_data_label, expand=False, fill=False)
    self._vbox_data.pack_start(self._parasite_data_entry, expand=False, fill=False)
    
    self.set_spacing(self._HBOX_SPACING)
    self.pack_start(self._vbox_name, expand=False, fill=False)
    self.pack_start(self._vbox_flags, expand=False, fill=False)
    self.pack_start(self._vbox_data, expand=False, fill=False)
    
    self._set_values(parasite)
    self._connect_changed_events()
  
  def _get_values(self):
    return (
      self._parasite_name_entry.get_text().decode(pgconstants.GTK_CHARACTER_ENCODING),
      self._parasite_flags_spin_button.get_value_as_int(),
      self._parasite_data_entry.get_text().decode(pgconstants.GTK_CHARACTER_ENCODING))
  
  def _set_values(self, parasite):
    self._should_invoke_parasite_changed_signal = False
    
    self._parasite_name_entry.set_text(
      parasite.name.encode(pgconstants.GTK_CHARACTER_ENCODING))
    self._parasite_flags_spin_button.set_value(parasite.flags)
    self._parasite_data_entry.set_text(
      parasite.data.encode(pgconstants.GTK_CHARACTER_ENCODING))
    
    self._should_invoke_parasite_changed_signal = True
  
  def _connect_changed_events(self):
    self._parasite_name_entry.connect("changed", self._on_parasite_changed)
    self._parasite_flags_spin_button.connect("value-changed", self._on_parasite_changed)
    self._parasite_data_entry.connect("changed", self._on_parasite_changed)
  
  def _on_parasite_changed(self, widget, *args, **kwargs):
    if self._should_invoke_parasite_changed_signal:
      self.emit("parasite-changed")


gobject.type_register(ParasiteBox)
