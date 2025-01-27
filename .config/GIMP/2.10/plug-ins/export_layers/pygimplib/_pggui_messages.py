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
This module defines:
* GTK exception dialog
* GTK generic message dialog
* wrapper for `sys.excepthook` that displays the GTK exception dialog when an
  unhandled exception is raised

This module should not be used directly. Use `pggui` as the contents of this
module are included in `pggui`.
"""

# NOTE: In order to allow logging errors as early as possible (before plug-in
# initialization), the `future` library is not imported in case some modules in
# the library are not available in the installed Python distribution and would
# thus cause an `ImportError` to be raised.

from __future__ import absolute_import, division, print_function, unicode_literals

str = unicode

import __builtin__
import functools
import sys
import traceback

try:
  import webbrowser
except ImportError:
  _webbrowser_module_found = False
else:
  _webbrowser_module_found = True

import pygtk
pygtk.require("2.0")
import gtk
import gobject
import pango

__all__ = [
  "display_error_message",
  "display_message",
  "add_gui_excepthook",
  "set_gui_excepthook",
  "set_gui_excepthook_parent",
  "set_gui_excepthook_additional_callback",
]


def display_error_message(
      title=None,
      app_name=None,
      parent=None,
      message_type=gtk.MESSAGE_ERROR,
      flags=gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
      message_markup=None,
      message_secondary_markup=None,
      details=None,
      display_details_initially=True,
      report_uri_list=None,
      report_description=None,
      button_stock_id=gtk.STOCK_CLOSE,
      button_response_id=gtk.RESPONSE_CLOSE,
      focus_on_button=False):
  """
  Display a message to alert the user about an error or an exception that
  occurred in the application.
  
  Parameters:
  
  * `title` - Message title.
  
  * `app_name` - Name of the application to use in the default contents of
    `message_secondary_markup`.
  
  * `parent` - Parent widget.
  
  * `message_type` - GTK message type (`gtk.MESSAGE_ERROR`, etc.).
  
  * `flags` - GTK dialog flags.
  
  * `message_markup` - Primary message text to display as markup.
  
  * `message_secondary_markup` - Secondary message text to display as markup.
  
  * `details` - Text to display in a box with details. If `None`, do not display
    any box.
  
  * `display_details_initially` - If `True`, display the details by default,
    otherwise hide them in an expander.
  
  * `report_uri_list` - List of (name, URL) pairs where the user can report
    the error. If no report list is desired, pass `None` or an empty sequence.
  
  * `report_description` - Text accompanying `report_uri_list`. If `None`, use
    default text. To suppress displaying the report description, pass an empty
    string.
  
  * `button_stock_id` - Stock ID of the button to close the dialog with.
  
  * `button_response_id` - Response ID of the button to close the dialog with.
  
  * `focus_on_button` - If `True`, focus on the button to close the dialog with.
    If `False`, focus on the box with details if `details` is not `None`,
    otherwise let the message dialog determine the focus widget.
  """
  if not ("_" in __builtin__.__dict__ or "_" in globals()):
    # This is a placeholder function until `gettext` is properly initialized.
    def _(str_):
      return str_
  else:
    # This is necessary since defining a local variable/function, even inside a
    # condition, obscures a global variable/function of the same name.
    _ = __builtin__.__dict__.get("_", None) or globals()["_"]
  
  if app_name is None:
    app_name = _("Plug-in")
  
  if message_markup is None:
    message_markup = (
      '<span font_size="large"><b>{0}</b></span>'.format(
        _("Oops. Something went wrong.")))
  
  if message_secondary_markup is None:
    message_secondary_markup = _(
      "{0} encountered an unexpected error and has to close. Sorry about that!").format(
        gobject.markup_escape_text(app_name))
  
  if report_description is None:
    report_description = _(
      "You can help fix this error by sending a report with the text "
      "in the details above to one of the following sites")
  
  dialog = gtk.MessageDialog(parent, type=message_type, flags=flags)
  dialog.set_transient_for(parent)
  if title is not None:
    dialog.set_title(title)
  
  dialog.set_markup(message_markup)
  dialog.format_secondary_markup(message_secondary_markup)
  
  if details is not None:
    expander = _get_details_expander(details, _("Details"))
    if display_details_initially:
      expander.set_expanded(True)
  else:
    expander = None
  
  if report_uri_list:
    vbox_labels_report = _get_report_link_buttons(
      report_uri_list, report_description, _("(right-click to copy link)"))
    dialog.vbox.pack_end(vbox_labels_report, expand=False, fill=False)
  
  if expander is not None:
    dialog.vbox.pack_start(expander, expand=False, fill=False)
  
  dialog.add_button(button_stock_id, button_response_id)
  
  if focus_on_button:
    button = dialog.get_widget_for_response(button_response_id)
    if button is not None:
      dialog.set_focus(button)
  else:
    if (expander is not None
        and expander.get_child() is not None
        and display_details_initially):
      dialog.set_focus(expander.get_child())
  
  dialog.show_all()
  response_id = dialog.run()
  dialog.destroy()
  
  return response_id


def _get_details_expander(details_text, details_label):
  expander = gtk.Expander()
  expander.set_use_markup(True)
  expander.set_label("<b>" + details_label + "</b>")
  
  scrolled_window = gtk.ScrolledWindow()
  scrolled_window.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
  scrolled_window.set_size_request(400, 200)
  scrolled_window.set_shadow_type(gtk.SHADOW_IN)
  
  exception_text_view = gtk.TextView()
  exception_text_view.set_editable(False)
  exception_text_view.set_wrap_mode(gtk.WRAP_WORD)
  exception_text_view.set_cursor_visible(False)
  exception_text_view.set_pixels_above_lines(1)
  exception_text_view.set_pixels_below_lines(1)
  exception_text_view.set_pixels_inside_wrap(0)
  exception_text_view.set_left_margin(5)
  exception_text_view.set_right_margin(5)
  exception_text_view.get_buffer().set_text(details_text)
  
  scrolled_window.add(exception_text_view)
  expander.add(scrolled_window)
  
  return expander


def _get_report_link_buttons(
      report_uri_list, report_description, label_report_text_instructions):
  if not report_uri_list:
    return None
  
  vbox_link_buttons = gtk.VBox(homogeneous=False)
  
  if report_description:
    label_report_text = report_description
    if not _webbrowser_module_found:
      label_report_text += " " + label_report_text_instructions
    label_report_text += ":"
    
    label_report = gtk.Label(label_report_text)
    label_report.set_alignment(0, 0.5)
    label_report.set_padding(3, 3)
    label_report.set_line_wrap(True)
    label_report.set_line_wrap_mode(pango.WRAP_WORD)
    vbox_link_buttons.pack_start(label_report, expand=False, fill=False)
  
  report_linkbuttons = []
  for name, uri in report_uri_list:
    linkbutton = gtk.LinkButton(uri, label=name)
    linkbutton.set_alignment(0, 0.5)
    report_linkbuttons.append(linkbutton)
  
  for linkbutton in report_linkbuttons:
    vbox_link_buttons.pack_start(linkbutton, expand=False, fill=False)
  
  if _webbrowser_module_found:
    # Apparently, GTK doesn't know how to open URLs on Windows, hence the custom
    # solution.
    for linkbutton in report_linkbuttons:
      linkbutton.connect(
        "clicked", lambda linkbutton: webbrowser.open_new_tab(linkbutton.get_uri()))
  
  return vbox_link_buttons


def display_message(
      message,
      message_type,
      title=None,
      parent=None,
      buttons=gtk.BUTTONS_OK,
      message_in_text_view=False,
      button_response_id_to_focus=None):
  """
  Display a generic message.
  
  Parameters:
  
  * `message` - The message to display.
  
  * `message_type` - GTK message type (`gtk.MESSAGE_INFO`, etc.).
  
  * `title` - Message title.
  
  * `parent` - Parent GUI element.
  
  * `buttons` - Buttons to display in the dialog.
  
  * `message_in_text_view` - If `True`, display text the after the first newline
    character in a text view.
  
  * `button_response_id_to_focus` - Response ID of the button to set as the
    focus. If `None`, the dialog determines which widget gets the focus.
  """
  dialog = gtk.MessageDialog(
    parent=parent,
    type=message_type,
    flags=gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
    buttons=buttons)
  dialog.set_transient_for(parent)
  if title is not None:
    dialog.set_title(title)
  
  messages = message.split("\n", 1)
  if len(messages) > 1:
    dialog.set_markup(gobject.markup_escape_text(messages[0]))
    
    if message_in_text_view:
      text_view = gtk.TextView()
      text_view.set_editable(False)
      text_view.set_wrap_mode(gtk.WRAP_WORD)
      text_view.set_cursor_visible(False)
      text_view.set_pixels_above_lines(1)
      text_view.set_pixels_below_lines(1)
      text_view.set_pixels_inside_wrap(0)
      text_view.get_buffer().set_text(messages[1])
      
      scrolled_window = gtk.ScrolledWindow()
      scrolled_window.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
      scrolled_window.set_property("height-request", 100)
      scrolled_window.set_shadow_type(gtk.SHADOW_IN)
      
      scrolled_window.add(text_view)
      
      vbox = dialog.get_message_area()
      vbox.pack_end(scrolled_window, expand=True, fill=True)
    else:
      dialog.format_secondary_markup(gobject.markup_escape_text(messages[1]))
  else:
    dialog.set_markup(gobject.markup_escape_text(message))
  
  if button_response_id_to_focus is not None:
    button = dialog.get_widget_for_response(button_response_id_to_focus)
    if button is not None:
      dialog.set_focus(button)
  
  dialog.show_all()
  response_id = dialog.run()
  dialog.destroy()
  
  return response_id


#===============================================================================

_gui_excepthook_parent = None
_gui_excepthook_additional_callback = lambda *args, **kwargs: False


def add_gui_excepthook(title, app_name, report_uri_list=None, parent=None):
  """
  Return a decorator that modifies `sys.excepthook` to display an error dialog
  for unhandled exceptions and terminates the application. `sys.excepthook` is
  restored once the decorated function finishes its execution.
  
  The dialog will not be displayed for exceptions which are not subclasses of
  `Exception` (such as `SystemExit` or `KeyboardInterrupt`).
  
  Parameters:
  
  * `title` - Dialog title.
  
  * `report_uri_list` - List of (name, URL) tuples where the user can report
    the error. If no report list is desired, pass `None` or an empty sequence.
  
  * `parent` - Parent GUI element.
  """
  global _gui_excepthook_parent
  
  _gui_excepthook_parent = parent
  
  def gui_excepthook(func):
    
    @functools.wraps(func)
    def func_wrapper(self, *args, **kwargs):
      
      def _gui_excepthook(exc_type, exc_value, exc_traceback):
        _gui_excepthook_generic(
          exc_type,
          exc_value,
          exc_traceback,
          orig_sys_excepthook,
          title,
          app_name,
          _gui_excepthook_parent,
          report_uri_list)
      
      orig_sys_excepthook = sys.excepthook
      sys.excepthook = _gui_excepthook
      
      func(self, *args, **kwargs)
      
      sys.excepthook = orig_sys_excepthook
    
    return func_wrapper
  
  return gui_excepthook


def set_gui_excepthook(title, app_name, report_uri_list=None, parent=None):
  """
  Modify `sys.excepthook` to display an error dialog for unhandled exceptions.
  
  The dialog will not be displayed for exceptions which are not subclasses of
  `Exception` (such as `SystemExit` or `KeyboardInterrupt`).
  
  For information about parameters, see `add_gui_excepthook()`.
  """
  global _gui_excepthook_parent
  
  _gui_excepthook_parent = parent
  
  def gui_excepthook(exc_type, exc_value, exc_traceback):
    _gui_excepthook_generic(
      exc_type,
      exc_value,
      exc_traceback,
      orig_sys_excepthook,
      title,
      app_name,
      _gui_excepthook_parent,
      report_uri_list)
  
  orig_sys_excepthook = sys.excepthook
  sys.excepthook = gui_excepthook


def set_gui_excepthook_parent(parent):
  """
  Set the parent GUI element to attach the exception dialog to when using
  `add_gui_excepthook()`. This function allows to modify the parent dynamically
  even after decorating a function with `add_gui_excepthook()`.
  """
  global _gui_excepthook_parent
  
  _gui_excepthook_parent = parent


def set_gui_excepthook_additional_callback(callback):
  """
  Set a callback to be executed at the beginning of exception handling. If the
  callback returns `True`, terminate exception handling at this point. Returning
  `True` consequently prevents the error dialog from being displayed and the
  application from being terminated.
  
  The callback takes the same parameters as `sys.excepthook`.
  """
  global _gui_excepthook_additional_callback
  
  _gui_excepthook_additional_callback = callback


def _gui_excepthook_generic(
      exc_type,
      exc_value,
      exc_traceback,
      orig_sys_excepthook,
      title,
      app_name,
      parent,
      report_uri_list):
  callback_result = _gui_excepthook_additional_callback(
    exc_type, exc_value, exc_traceback)
  
  if callback_result:
    return
  
  orig_sys_excepthook(exc_type, exc_value, exc_traceback)
  
  if issubclass(exc_type, Exception):
    exception_message = "".join(
      traceback.format_exception(exc_type, exc_value, exc_traceback))
    
    display_error_message(
      title=title,
      app_name=app_name,
      parent=parent,
      details=exception_message,
      report_uri_list=report_uri_list)
    
    # Make sure to quit the application since unhandled exceptions can
    # mess up the application state.
    if gtk.main_level() > 0:
      gtk.main_quit()
