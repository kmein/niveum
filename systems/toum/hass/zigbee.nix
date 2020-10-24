{ config, pkgs, lib, ... }:
let
  inherit (import <stockholm/lib>) genid;
  inherit (import <niveum/lib>) localAddresses;
  zigbee2mqttDevice = "/dev/ttyACM0";

  zigbee2mqttConfig = {
    permit_join = false;
    homeassistant = true;
    serial.port = zigbee2mqttDevice;
    mqtt = {
      discovery = true;
      base_topic = "zigbee";
      server = "mqtt://${localAddresses.toum}"; # Rasperry local IP
      user = "albrecht";
      password = lib.strings.fileContents <system-secrets/mosquitto>;
    };
  };
  zigbee2mqtt_cfg = pkgs.writeText "zigbee2mqtt.json" (builtins.toJSON zigbee2mqttConfig);
in
{
  disabledModules = [
    "services/misc/home-assistant.nix"
  ];

  imports = [
    <nixpkgs-unstable/nixos/modules/services/misc/home-assistant.nix>
    <nixpkgs-unstable/nixos/modules/services/misc/zigbee2mqtt.nix>
  ];

  /*
  ids = {
    uids.zigbee2mqtt = genid "zigbee2mqtt";
    gids.zigbee2mqtt = genid "zigbee2mqtt";
  };
  services.zigbee2mqtt = {
    enable = true;
    config = zigbee2mqttConfig;
    package = pkgs.unstable.zigbee2mqtt;
  };
  */


  system.activationScripts.installZigbee = ''
    install -d /var/lib/zigbee2mqtt
    install ${zigbee2mqtt_cfg} /var/lib/zigbee2mqtt/configuration.yaml
  '';

  # hack to restart docker container on config change
  systemd.services.docker-zigbee2mqtt.environment.cfg = zigbee2mqtt_cfg;

  docker-containers.zigbee2mqtt = {
    image = "koenkk/zigbee2mqtt";
    extraDockerOptions = [
      "--device=${zigbee2mqttDevice}:${zigbee2mqttDevice}"
    ];
    volumes = ["/var/lib/zigbee2mqtt:/app/data"];
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
