{ config, pkgs, lib, ... }:
let
  inherit (import <stockholm/lib>) genid;
in
{
  disabledModules = [
    "services/misc/home-assistant.nix"
  ];

  imports = [
    <nixos-unstable/nixos/modules/services/misc/home-assistant.nix>
    <nixos-unstable/nixos/modules/services/misc/zigbee2mqtt.nix>
  ];

  ids = {
    uids.zigbee2mqtt = genid "zigbee2mqtt";
    gids.zigbee2mqtt = genid "zigbee2mqtt";
  };

  services.zigbee2mqtt = {
    enable = true;
    package = pkgs.unstable.zigbee2mqtt;
    config = {
      permit_join = true;
      homeassistant = true;
      serial.port = "/dev/ttyACM0";
      mqtt = {
        discovery = true;
        base_topic = "zigbee";
        server = "mqtt://192.168.178.24"; # Rasperry local IP
        user = "albrecht";
        password = lib.strings.fileContents <system-secrets/mosquitto>;
      };
    };
  };

  services.mosquitto = {
    enable = true;
    host = "0.0.0.0";
    allowAnonymous = false;
    checkPasswords = true;
    users."albrecht" = {
      password = lib.strings.fileContents <system-secrets/mosquitto>;
      acl = [ "topic readwrite #" ];
    };
  };

  environment.systemPackages = [ pkgs.mosquitto ];

  services.home-assistant = {
    config = {
      switch = [
        {
          platform = "mqtt";
          name = "zigbee2mqtt_join";
          state_topic = "/zigbee2mqtt/bridge/config/permit_join";
          command_topic = "/zigbee2mqtt/bridge/config/permit_join";
          payload_on = "true";
          payload_off = "false";
        }
      ];
      timer.zigbee_permit_join = {
        name = "Zigbee Time remaining";
        duration = 120;
      };
      automation = [
        # Automation to start timer when enable join is turned on
        {
          id = "zigbee_join_enabled";
          alias = "";
          hide_entity = "true";
          trigger = {
            platform = "state";
            entity_id = "switch.zigbee2mqtt_join";
            to = "on";
          };
          action = {
            service = "timer.start";
            entity_id = "timer.zigbee_permit_join";
          };
        }
        # Automation to stop timer when switch turned off and turn off switch when timer finished
        {
          id = "zigbee_join_disabled";
          hide_entity = "true";
          trigger = [
            {
              platform = "event";
              event_type = "timer.finished";
              event_data.entity_id = "timer.zigbee_permit_join";
            }
            {
              platform = "state";
              entity_id = "switch.zigbee2mqtt_join";
              to = "off";
            }
          ];
          action = [
            {
              service = "timer.cancel";
              data.entity_id = "timer.zigbee_permit_join";
            }
            {
              service = "switch.turn_off";
              entity_id = "switch.zigbee2mqtt_join";
            }
          ];
        }
      ];
    };
  };
}
