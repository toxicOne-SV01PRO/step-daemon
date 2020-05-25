from __future__ import absolute_import

import serial
import flask
import octoprint.plugin
import os.path
import time

from os import path

from .StepdThread import StepdThread

"""
octoprint.plugin.EventHandlerPlugin - for events

octoprint.printer.factory - used to create the global printer object

or better, use octoprint.comm.transport.serial.factory to create the serial connection

octoprint.comm.protocol.action for detecting the "stedp started action", stepd:start

OctoPrintPlugin.get_plugin_data_folder for data folder
"""

class StepdPlugin(octoprint.plugin.SettingsPlugin,
                  octoprint.plugin.AssetPlugin,
                  octoprint.plugin.TemplatePlugin,
                  octoprint.plugin.SimpleApiPlugin,
                  octoprint.plugin.StartupPlugin):

  def __init__(self, **kwargs):
    super().__init__(**kwargs)

    self.thread = None

  ##~~ SettingsPlugin mixin

  def get_settings_defaults(self):
    return dict(
      # put your plugin's default settings here
    )

  ##~~ SimpleApiPlugin mixin

  def get_api_commands(self):
    return dict(
      update=[]
    )

  def on_api_command(self, command, data):
    if command == "update":
      self.thread.update_stepd()

  def on_api_get(self, request):
    return flask.jsonify(foo="bar")

  ##~~ TemplatePlugin mixin

  def get_template_vars(self):
    return dict(
      status=self.get_current_status(),
      config_path=os.path.join(self.get_plugin_data_folder(), 'config.conf')
    )

  def get_template_configs(self):
    return [
      #dict(type="navbar", custom_bindings=False),
      #dict(type="settings", custom_bindings=False)
    ]

  ##~~ AssetPlugin mixin

  def get_assets(self):
    # Define your plugin's asset files to automatically include in the
    # core UI here.
    return dict(
      js=["js/stepd.js"],
      css=["css/stepd.css"],
      less=["less/stepd.less"]
    )

  ##~~ Softwareupdate hook

  def get_update_information(self):
    # Define the configuration for your plugin to use with the Software Update
    # Plugin here. See https://docs.octoprint.org/en/master/bundledplugins/softwareupdate.html
    # for details.
    return dict(
      stepd=dict(
        displayName="Stepd Plugin",
        displayVersion=self._plugin_version,

        # version check: github repository
        type="github_release",
        user="colinrgodsey",
        repo="OctoPrint-stepd",
        current=self._plugin_version,

        # update method: pip
        pip="https://github.com/colinrgodsey/OctoPrint-stepd/archive/{target_version}.zip"
      )
    )

  ##~~ Serial factory hook
  def serial_factory_hook(self, comm_instance, port, baudrate, read_timeout, *args, **kwargs):
    comm_instance._log("Connecting to Step Daemon")

    wait_s = 10
    while not self.thread or not self.thread.running:
      if wait_s <= 0:
        comm_instance._log("Failed waiting for stepd...")
        break

      wait_s -= 1
      time.sleep(1)

    time.sleep(2)

    port = '/tmp/pty-stepd-client'

    return serial.Serial(str(port), 230400, writeTimeout=10000)

  ##~~ StartupPlugin mixin
  def on_after_startup(self):
    self.thread = StepdThread(self.get_plugin_data_folder(), self._logger)
    self.thread.start()

  def get_current_status(self):
    if not self.thread:
      return "Starting..."
    elif self.thread.running:
      return "Running..."
    elif self.thread.is_alive():
      return "Updating..."
    else:
      return "Server has crashed. Please restart OctoPrint."







