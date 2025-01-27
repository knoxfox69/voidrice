# -*- coding: utf-8 -*-
#
# This file is part of Export Layers.
#
# Copyright (C) 2013-2019 khalim19 <khalim19@gmail.com>
#
# Export Layers is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Export Layers is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Export Layers.  If not, see <https://www.gnu.org/licenses/>.

"""
This module defines a widget to display a label for the purposes of displaying
inline messages to the user.
"""

from __future__ import absolute_import, division, print_function, unicode_literals
from future.builtins import *

import os

import pygtk
pygtk.require("2.0")
import gtk
import gobject
import pango

import gimp

from export_layers.pygimplib import pgconstants
from export_layers.pygimplib import pggui
from export_layers.pygimplib import pginvocation


class MessageLabel(gtk.HBox):
  """
  This class defines a widget to display a label, and optionally additional
  information in a popup below the label. The popup is also available if the
  label text does not fit the width of the parent widget.
  """
  
  _MESSAGE_AND_MORE_BUTTON_SPACING = 2
  _MORE_BUTTON_LABEL_AND_ARROW_SPACING = 5
  
  _TEXT_VIEW_MARGIN = 3
  
  _POPUP_WIDTH = 400
  _MAX_POPUP_HEIGHT = 200
  
  def __init__(self):
    super().__init__(homogeneous=False)
    
    self._label_text = ""
    self._popup_text_lines = []
    self._message_type = None
    self._clear_delay = None
    
    self._init_gui()
    
    self._popup_hide_context = pggui.PopupHideContext(self._popup_more, self._button_more)
    
    self._label_message.connect("size-allocate", self._on_label_message_size_allocate)
    self._button_more.connect("clicked", self._on_button_more_clicked)
    
    self._popup_more.connect("show", self._on_popup_more_show)
    self._popup_more.connect("hide", self._on_popup_more_hide)
    
    wigets_to_exclude_from_hiding_popup_with_button_press = [
      self._popup_more,
      self._scrolled_window_more.get_hscrollbar(),
      self._scrolled_window_more.get_vscrollbar()]
    
    for widget in wigets_to_exclude_from_hiding_popup_with_button_press:
      self._popup_hide_context.exclude_widget_from_hiding_with_button_press(widget)
  
  def set_text(self, text, message_type=gtk.MESSAGE_ERROR, clear_delay=None):
    """
    Set the `text` of the label. The text is displayed in bold style.
    
    If the text is too wide to fit the label or the text has multiple lines,
    ellipsize the label and display a button that displays a popup containing
    the full text when clicked. Only the first line is displayed in the label.
    
    If `message_type` is `gtk.MESSAGE_ERROR`, use the red font color.
    For other message types, use the font color assigned by the current theme.
    
    If `clear_delay` is not `None` and `message_type` is not
    `gtk.MESSAGE_ERROR`, make the message automatically disappear after the
    specified delay in milliseconds. The timer is stopped if the popup is
    displayed and restarted if the popup gets hidden.
    """
    if not text:
      self._label_text = ""
      self._popup_text_lines = []
      self._label_message.set_text(self._label_text)
      return
    
    lines = text.strip().split("\n")
    
    first_line = lines[0]
    first_line = first_line[0].upper() + first_line[1:]
    if not first_line.endswith("."):
      first_line += "."
    
    self._label_text = first_line
    self._popup_text_lines = lines[1:]
    self._message_type = message_type
    self._clear_delay = clear_delay
    
    if message_type == gtk.MESSAGE_ERROR:
      self._label_message.set_markup(
        '<span foreground="red"><b>{}</b></span>'.format(gobject.markup_escape_text(
          self._label_text.encode(pgconstants.GTK_CHARACTER_ENCODING))))
      self._timeout_remove_strict(self._clear_delay, self.set_text)
    else:
      self._label_message.set_markup(
        "<b>{}</b>".format(gobject.markup_escape_text(
          self._label_text.encode(pgconstants.GTK_CHARACTER_ENCODING))))
      self._timeout_add_strict(self._clear_delay, self.set_text, None)
  
  def _init_gui(self):
    self._label_message = gtk.Label()
    self._label_message.set_alignment(0.0, 0.5)
    self._label_message.set_ellipsize(pango.ELLIPSIZE_END)
    
    self._label_button_more = gtk.Label(_("_More"))
    self._label_button_more.set_use_underline(True)
    
    self._hbox_button_more = gtk.HBox()
    self._hbox_button_more.set_spacing(self._MORE_BUTTON_LABEL_AND_ARROW_SPACING)
    self._hbox_button_more.pack_start(
      self._label_button_more, expand=True, fill=True)
    self._hbox_button_more.pack_start(
      gtk.Arrow(gtk.ARROW_DOWN, gtk.SHADOW_IN), expand=False, fill=False)
    
    self._button_more = gtk.Button()
    self._button_more.set_relief(gtk.RELIEF_NONE)
    self._button_more.add(self._hbox_button_more)
    self._button_more.show_all()
    self._button_more.hide()
    self._button_more.set_no_show_all(True)
    
    self._text_view_more = gtk.TextView()
    self._text_view_more.set_wrap_mode(gtk.WRAP_WORD)
    self._text_view_more.set_left_margin(self._TEXT_VIEW_MARGIN)
    self._text_view_more.set_right_margin(self._TEXT_VIEW_MARGIN)
    self._text_view_more.set_editable(False)
    
    self._scrolled_window_more = gtk.ScrolledWindow()
    self._scrolled_window_more.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_NEVER)
    self._scrolled_window_more.set_shadow_type(gtk.SHADOW_ETCHED_IN)
    self._scrolled_window_more.add(self._text_view_more)
    
    self._popup_more = gtk.Window(type=gtk.WINDOW_POPUP)
    self._popup_more.set_resizable(False)
    self._popup_more.set_type_hint(gtk.gdk.WINDOW_TYPE_HINT_TOOLTIP)
    self._popup_more.set_property("width-request", self._POPUP_WIDTH)
    self._popup_more.add(self._scrolled_window_more)
    self._popup_more.show_all()
    self._popup_more.hide()
    
    self.set_spacing(self._MESSAGE_AND_MORE_BUTTON_SPACING)
    self.pack_start(self._label_message, expand=True, fill=True)
    self.pack_start(self._button_more, expand=False, fill=False)
  
  def _on_label_message_size_allocate(self, label, allocation):
    if (pggui.get_label_full_text_width(self._label_message) > self.get_allocation().width
        or len(self._popup_text_lines) >= 1):
      self._button_more.show()
    else:
      self._button_more.hide()
  
  def _on_button_more_clicked(self, button):
    lines = list(self._popup_text_lines)
    
    if (pggui.get_label_full_text_width(self._label_message)
        > self._label_message.get_allocation().width):
      lines.insert(0, self._label_text)
    
    text = "\n".join(lines).strip()
    
    text_buffer = gtk.TextBuffer()
    text_buffer.set_text(text.encode(pgconstants.GTK_CHARACTER_ENCODING))
    self._text_view_more.set_buffer(text_buffer)
    
    self._popup_more.move(*pggui.get_position_below_widget(self))
    self._popup_more.show()
  
  def _on_popup_more_show(self, popup):
    self._popup_hide_context.connect_button_press_events_for_hiding()
    
    self._popup_more.set_screen(self._button_more.get_screen())
    
    if self._message_type != gtk.MESSAGE_ERROR:
      self._timeout_remove_strict(self._clear_delay, self.set_text)
  
  def _on_popup_more_hide(self, popup):
    self._popup_hide_context.disconnect_button_press_events_for_hiding()
    
    if self._message_type != gtk.MESSAGE_ERROR:
      self._timeout_add_strict(self._clear_delay, self.set_text, None)
  
  def _timeout_add_strict(self, delay, func, *args, **kwargs):
    if self._should_clear_text_after_delay(delay):
      pginvocation.timeout_add_strict(delay, func, None, *args, **kwargs)
  
  def _timeout_remove_strict(self, delay, func):
    if self._should_clear_text_after_delay(delay):
      pginvocation.timeout_remove_strict(func)
  
  def _should_clear_text_after_delay(self, clear_delay):
    return (
      clear_delay is not None
      and clear_delay > 0
      and not (os.name == "nt" and ((2, 10, 0) <= gimp.version < (2, 10, 6))))


gobject.type_register(MessageLabel)
