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

from __future__ import absolute_import, division, print_function, unicode_literals

import inspect
import os
import sys
import traceback

_PYGIMPLIB_DIRPATH = os.path.dirname(inspect.getfile(inspect.currentframe()))

try:
  import gimp
except ImportError:
  _gimp_dependent_modules_imported = False
else:
  _gimp_dependent_modules_imported = True

from . import pglogging


if _gimp_dependent_modules_imported:
  # Enable logging as early as possible to capture any unexpected errors (such
  # as missing modules) before pygimplib is fully initialized.
  pglogging.log_output(
    log_mode=pglogging.LOG_EXCEPTIONS_ONLY,
    log_dirpaths=[os.path.dirname(_PYGIMPLIB_DIRPATH), _PYGIMPLIB_DIRPATH],
    log_stdout_filename=None,
    log_stderr_filename="error.log",
    log_header_title="pygimplib")

if _gimp_dependent_modules_imported:
  from . import _pggui_messages
  
  _pggui_messages.set_gui_excepthook(title=None, app_name=None)


def _setup_import_of_external_lib_modules(dirpath):
  """
  Add directory paths containing external libraries for pygimplib to `sys.path`
  so that modules from these external libraries can be imported as system
  modules (i.e. without using absolute or explicit relative imports).
  
  Modules with the same name that are already installed system-wide override the
  external library modules from pygimplib.
  """
  for filename in os.listdir(dirpath):
    external_libs_dirpath = os.path.join(dirpath, filename)
    if os.path.isdir(external_libs_dirpath) and external_libs_dirpath not in sys.path:
      sys.path.append(external_libs_dirpath)


_setup_import_of_external_lib_modules(os.path.join(_PYGIMPLIB_DIRPATH, "lib"))


from future.builtins import *

import collections
import gettext

if _gimp_dependent_modules_imported:
  import gimpenums
  import gimpui
  
  from . import pgconstants
  from . import pggui
  from . import pgsetting
  from . import pgsettinggroup
  from . import pgsettingsources
  from . import pgsettingpdb


config = None


class _Config(object):
  
  def __init__(self):
    super().__setattr__("_config", {})
    self._config["_can_modify_config"] = True
  
  def __setattr__(self, name, value):
    if self._can_modify_config:
      self._config[name] = value
    else:
      raise TypeError("cannot modify configuration after plug-in initialization")
  
  def __getattr__(self, name):
    if name not in self._config:
      raise AttributeError("configuration entry '{}' not found".format(name))
    
    attr = self._config[name]
    
    if callable(attr):
      return attr()
    else:
      return attr
  
  def __hasattr__(self, name):
    return name in self._config


def _init_config():
  global config
  
  if config is not None:
    return
  
  def _get_domain_name():
    if config.PLUGIN_NAME == config._DEFAULT_PLUGIN_NAME:
      return "gimp20-python"
    else:
      return "gimp-plugin-" + config.PLUGIN_NAME.replace("_", "-")
  
  config = _Config()
  
  config._DEFAULT_PLUGIN_NAME = "gimp_plugin"
  config.PLUGIN_NAME = config._DEFAULT_PLUGIN_NAME
  config.PLUGIN_TITLE = lambda: config.PLUGIN_NAME
  config.PLUGIN_VERSION = "1.0"
  
  config.LOCALE_DIRPATH = lambda: os.path.join(config.PLUGINS_DIRPATH, "locale")
  config.DOMAIN_NAME = _get_domain_name
  
  config.BUG_REPORT_URL_LIST = []
  
  config.PLUGINS_DIRPATH = os.path.dirname(os.path.dirname(_PYGIMPLIB_DIRPATH))
  
  if _gimp_dependent_modules_imported:
    config.LOG_MODE = pglogging.LOG_EXCEPTIONS_ONLY
  
  gettext.install(config.DOMAIN_NAME, config.LOCALE_DIRPATH, unicode=True)
  
  _init_config_builtin(config)


def _init_config_builtin(config):
  
  def _get_setting_source_name():
    if config.PLUGIN_NAME.startswith("plug_in"):
      return config.PLUGIN_NAME
    else:
      return "plug_in_" + config.PLUGIN_NAME
  
  config.SOURCE_SESSION_NAME = _get_setting_source_name()
  config.SOURCE_PERSISTENT_NAME = _get_setting_source_name()
  
  config.DEFAULT_LOGS_DIRPATH = os.path.dirname(_PYGIMPLIB_DIRPATH)
  
  config.PLUGINS_LOG_DIRPATHS = []
  config.PLUGINS_LOG_DIRPATHS.append(config.DEFAULT_LOGS_DIRPATH)
  
  if _gimp_dependent_modules_imported:
    plugins_dirpath_alternate = os.path.join(gimp.directory, "plug-ins")
    if plugins_dirpath_alternate != config.DEFAULT_LOGS_DIRPATH:
      # Add `[user directory]/[GIMP directory]/plug-ins` as another log path in
      # case the plug-in was installed system-wide and there is no permission to
      # create log files there.
      config.PLUGINS_LOG_DIRPATHS.append(plugins_dirpath_alternate)
  
  config.PLUGINS_LOG_STDOUT_DIRPATH = config.DEFAULT_LOGS_DIRPATH
  config.PLUGINS_LOG_STDERR_DIRPATH = config.DEFAULT_LOGS_DIRPATH
  
  config.PLUGINS_LOG_STDOUT_FILENAME = "output.log"
  config.PLUGINS_LOG_STDERR_FILENAME = "error.log"
  
  config.GIMP_CONSOLE_MESSAGE_DELAY_MILLISECONDS = 50


def _init_config_builtin_delayed(config):
  if _gimp_dependent_modules_imported:
    config.SOURCE_SESSION = pgsettingsources.SessionWideSettingSource(
      config.SOURCE_SESSION_NAME)
    config.SOURCE_PERSISTENT = pgsettingsources.PersistentSettingSource(
      config.SOURCE_PERSISTENT_NAME)


_init_config()

_is_initialized = False


def init():
  global _is_initialized
  
  if _is_initialized:
    return
  
  _init_config_builtin(config)
  _init_config_builtin_delayed(config)
  
  gettext.install(config.DOMAIN_NAME, config.LOCALE_DIRPATH, unicode=True)
  
  if (_gimp_dependent_modules_imported
      or config.LOG_MODE != pglogging.LOG_OUTPUT_GIMP_CONSOLE):
    pglogging.log_output(
      config.LOG_MODE, config.PLUGINS_LOG_DIRPATHS,
      config.PLUGINS_LOG_STDOUT_FILENAME, config.PLUGINS_LOG_STDERR_FILENAME,
      config.PLUGIN_TITLE, config.GIMP_CONSOLE_MESSAGE_DELAY_MILLISECONDS)
  
  _is_initialized = True


if _gimp_dependent_modules_imported:
  
  _procedures = collections.OrderedDict()
  _procedures_names = collections.OrderedDict()
  
  def procedure(**kwargs):
    """
    This function is a decorator that installs the wrapped function as a GIMP
    procedure. The procedure can then be accessed via the GIMP procedural
    database (PDB) and optionally from the GIMP user interface.
    
    The function name is used as the procedure name as found in the GIMP PDB.
    
    The following keyword arguments are accepted:
    
    * `blurb` - Short description of the procedure.
    
    * `description` - More detailed information about the procedure.
    
    * `author` - Author of the plug-in.
    
    * `copyright_holder` - Copyright holder of the plug-in.
    
    * `date` - Dates (usually years) at which the plug-in development was
      active.
    
    * `menu_name` - name of the menu entry in the GIMP user interface.
    
    * `menu_path` - path of the menu entry in the GIMP user interface.
    
    * `image_types` - image types to which the procedure applies (e.g. RGB or
      indexed). By default, the procedure can be run for images of any type.
    
    * `parameters` - procedure parameters. This is a list of tuples of three
      elements: `(PDB type, name, description)`. Alternatively, you may pass
      a `SettingGroup` instance or a list of `SettingGroup` instances containing
      plug-in settings.
    
    * `return_values` - return values of the procedure, usable when calling the
      procedure programmatically. The format of `return_values` is the same as
      `parameters`.
    
    Example:
    
      \@pygimplib.procedure(
        blurb="Export layers as separate images",
        author="John Doe",
        menu_name=_("E_xport Layers..."),
        menu_path="<Image>/File/Export",
        parameters=[
          (gimpenums.PDB_INT32, "run-mode", "The run mode"),
          (gimpenums.PDB_IMAGE, "image", "The current image"),
          (gimpenums.PDB_STRING, "dirpath", "The export directory path")]
      )
      def plug_in_export_layers(run_mode, image, *args):
        ...
    """
    
    def procedure_wrapper(procedure):
      _procedures[procedure] = kwargs
      _procedures_names[procedure.__name__] = procedure
      return procedure
    
    return procedure_wrapper
  
  def main():
    """
    Enable the installation and execution of GIMP procedures.
    """
    gimp.main(None, None, _query, _run)
  
  def _install_procedure(
        procedure,
        blurb="",
        description="",
        author="",
        copyright_notice="",
        date="",
        menu_name="",
        menu_path=None,
        image_types="*",
        parameters=None,
        return_values=None):
    
    def _get_pdb_params(params):
      pdb_params = []
      
      if params:
        has_settings = isinstance(
          params[0], (pgsetting.Setting, pgsettinggroup.SettingGroup))
        if has_settings:
          pdb_params = pgsettingpdb.create_params(*params)
        else:
          pdb_params = params
      
      return pdb_params
    
    gimp.install_procedure(
      procedure.__name__,
      blurb,
      description,
      author,
      copyright_notice,
      date,
      menu_name,
      image_types,
      gimpenums.PLUGIN,
      _get_pdb_params(parameters),
      _get_pdb_params(return_values))
    
    if menu_path:
      gimp.menu_register(procedure.__name__, menu_path)
  
  def _query():
    gimp.domain_register(config.DOMAIN_NAME, config.LOCALE_DIRPATH)
    
    for procedure, kwargs in _procedures.items():
      _install_procedure(procedure, **kwargs)
  
  def _run(procedure_name, procedure_params):
    if config.PLUGIN_NAME == config._DEFAULT_PLUGIN_NAME:
      config.PLUGIN_NAME = procedure_name
    
    init()
    
    config._can_modify_config = False
    
    procedure = _add_gui_excepthook(
      _procedures_names[procedure_name], procedure_params[0])
    
    if hasattr(gimpui, "gimp_ui_init"):
      gimpui.gimp_ui_init()
    
    procedure(*procedure_params)
  
  def _add_gui_excepthook(procedure, run_mode):
    if run_mode == gimpenums.RUN_INTERACTIVE:
      pggui.set_gui_excepthook_additional_callback(
        _display_message_on_setting_value_error)
      
      add_gui_excepthook_func = pggui.add_gui_excepthook(
        title=config.PLUGIN_TITLE,
        app_name=config.PLUGIN_TITLE,
        report_uri_list=config.BUG_REPORT_URL_LIST)
      
      return add_gui_excepthook_func(procedure)
    else:
      return procedure
  
  def _display_message_on_setting_value_error(exc_type, exc_value, exc_traceback):
    if issubclass(exc_type, pgsetting.SettingValueError):
      gimp.message(str(exc_value).encode(pgconstants.GIMP_CHARACTER_ENCODING))
      return True
    else:
      return False
