{pkgs, ...}: let
  port = 8123;
  mosquittoPort = 1883;
  zigbee2mqttPort = 7977;
in {
  networking.firewall.allowedTCPPorts = [port zigbee2mqttPort];

  services.zigbee2mqtt = {
    enable = true;
    settings = {
      serial.port = "/dev/ttyACM0";
      permit_join = true;
      homeassistant = true;
      frontend = {
        port = zigbee2mqttPort;
        host = "0.0.0.0";
      };
      experimental.new_api = true;
      mqtt = {
        discovery = true;
        base_topic = "zigbee";
        server = "mqtt://localhost:${toString mosquittoPort}";
        user = "as59";
        password = "as59-mqtt";
      };
    };
  };

  services.mosquitto = {
    enable = true;
    listeners = [
      {
        acl = [];
        users.as59 = {
          acl = ["readwrite #"];
          password = "as59-mqtt";
        };
        port = mosquittoPort;
      }
    ];
  };

  environment.systemPackages = [pkgs.mosquitto];

  services.nginx.virtualHosts."home.kmein.r" = {
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString port}";
    };
  };

  services.home-assistant = {
    enable = true;
    configWritable = true;
    lovelaceConfigWritable = true;
    config = let
    in {
      homeassistant = {
        name = "Home";
        time_zone = "Europe/Berlin";
        latitude = "52.46187";
        longitude = "13.41489";
        elevation = 90;
        unit_system = "metric";
        # customize = friendly_names;
      };
      config = {};
      sun.elevation = 66;
      discovery = {};
      frontend = {};
      http = {};
      weather = [
        #{
        #  platform = "openweathermap";
        #  api_key = "xxx"; # TODO put into secrets
        #}
      ];
      system_health = {};
      history = {};
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
      automation = [
        {
          id = "zigbee_join_enabled";
          alias = "";
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
        {
          id = "zigbee_join_disabled";
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
      timer.zigbee_permit_join = {
        name = "Zigbee Time remaining";
        duration = 120;
      };
      mobile_app = {};
      sensor = [
        {
          platform = "dwd_weather_warnings";
          region_name = "Berlin";
        }
        # Sensor for monitoring the bridge state
        {
          platform = "mqtt";
          name = "Zigbee2mqtt Bridge state";
          state_topic = "/zigbee2mqtt/bridge/state";
          icon = "mdi:router-wireless";
        }
        # Sensor for Showing the Zigbee2mqtt Version
        {
          platform = "mqtt";
          name = "Zigbee2mqtt Version";
          state_topic = "/zigbee2mqtt/bridge/config";
          value_template = "{{ value_json.version }}";
          icon = "mdi:zigbee";
        }
        # Sensor for Showing the Coordinator Version
        {
          platform = "mqtt";
          name = "Coordinator Version";
          state_topic = "/zigbee2mqtt/bridge/config";
          value_template = "{{ value_json.coordinator }}";
          icon = "mdi:chip";
        }
      ];
      mqtt = {
        broker = "localhost";
        port = mosquittoPort;
        client_id = "home-assistant";
        username = "gg23";
        password = "gg23-mqtt";
        keepalive = 60;
        protocol = 3.1;

        discovery = true;
        birth_message = {
          topic = "/hass/status";
          payload = "online";
        };
        will_message = {
          topic = "/hass/status";
          payload = "offline";
        };
      };
    };
  };

  # virtualisation.oci-containers = {
  #   backend = "podman";
  #   containers.homeassistant = {
  #     volumes = ["home-assistant:/config"];
  #     environment.TZ = "Europe/Berlin";
  #     image = "ghcr.io/home-assistant/home-assistant:stable";
  #     extraOptions = [
  #       "--network=host"
  #       # "--device=/dev/ttyUSB0:/dev/ttyACM0"  # Example, change this to match your own hardware
  #     ];
  #   };
  # };
}
