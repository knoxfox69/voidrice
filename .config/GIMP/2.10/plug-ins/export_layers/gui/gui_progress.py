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
This module defines a progress indicator for processed items.
"""

from __future__ import absolute_import, division, print_function, unicode_literals
from future.builtins import *

import pygtk
pygtk.require("2.0")
import gtk

import gimp

from export_layers.pygimplib import pgutils


class ItemProgressIndicator(object):
  """
  This class controls a single progress bar to indicate the number of processed
  items and the status of the currently processed item.
  """
  
  def __init__(self, progress_bar, progress_updater):
    self._progress_bar = progress_bar
    self._progress_updater = progress_updater
    
    self._progress_callback = None
    self._progress_set_fraction_func = self._progress_set_fraction
  
  def install_progress_for_status(
        self, progress_set_value=None, progress_reset_value=None):
    """
    Initialize the progress bar for the current item status to update according
    to GIMP PDB calls.
    """
    if gimp.version >= (2, 9):
      return
    
    if progress_set_value is not None:
      self._progress_set_fraction_func = progress_set_value
    else:
      self._progress_set_fraction_func = self._progress_set_fraction
    
    if progress_reset_value is None:
      def progress_reset_value_default(*args):
        self._progress_set_fraction(0.0)
      
      progress_reset_value = progress_reset_value_default
    
    self._progress_callback = gimp.progress_install(
      progress_reset_value,
      progress_reset_value,
      pgutils.empty_func,
      self._progress_set_value_for_status)
  
  def uninstall_progress_for_status(self):
    """
    Reset the progress bar for the current item status so that it no longer
    updates according to GIMP PDB calls.
    """
    if gimp.version >= (2, 9):
      return
    
    if self._progress_callback is not None:
      gimp.progress_uninstall(self._progress_callback)
      self._progress_callback = None
  
  def _progress_set_fraction(self, fraction):
    self._progress_bar.set_fraction(fraction)
    while gtk.events_pending():
      gtk.main_iteration()
  
  def _progress_set_value_for_status(self, fraction):
    relative_fraction = (
      (self._progress_updater.num_finished_tasks / self._progress_updater.num_total_tasks)
      + (fraction / self._progress_updater.num_total_tasks))
    
    self._progress_set_fraction_func(relative_fraction)
