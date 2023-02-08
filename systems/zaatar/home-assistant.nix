{config, ...}: let
  port = 8123;
in {
  networking.firewall.allowedTCPPorts = [port];

  services.nginx.virtualHosts."home.kmein.r" = {
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString config.services.home-assistant.http.server_port}";
    };
  };

  services.home-assistant = {
    enable = true;
    config = {
      frontend = {};
      sun = {};
      # TODO calendar = { platform = "caldav"; url = "TODO"; username = "TODO"; password = "TODO"; }
      config = {};
      mobile_app = {};
      network = {};
      homeassistant = {
        name = "Home";
        latitude = "";
      };
      auth.users = [
        {
          id = "aa6284a151da4a9c91b4a18d86a5788b";
          group_ids = ["system-admin"];
          is_owner = true;
          is_active = true;
          name = "kmein";
        }
      ];
      "core.area_registry".areas = [
        {
          id = "living_room";
          name = "Saal";
        }
        {
          id = "kitchen";
          name = "Küche";
        }
        {
          id = "bedroom";
          name = "Zimmer";
        }
        {
          id = "corridor";
          name = "Korridor";
        }
      ];
      default_config = {};
      http.server_port = 8124;
      automation = [
        {
          id = "1647008057816";
          alias = "Abend";
          description = "";
          trigger = [
            {
              platform = "sun";
              event = "sunset";
              offset = 15;
            }
          ];
          condition = [];
          mode = "single";
          action = [
            {
              service = "scene.turn_on";
              target.entity_id = "scene.abend";
              metadata = {};
            }
          ];
        }
        {
          id = "1647205852858";
          alias = "Come home";
          description = "";
          trigger = [
            {
              platform = "device";
              device_id = "7279f87d95574352ff7e9cc69c325a63";
              domain = "device_tracker";
              entity_id = "device_tracker.moto_g_7_power";
              type = "enters";
              zone = "zone.home";
            }
          ];
          condition = [
            {
              condition = "sun";
              after = "sunset";
              after_offset = 15;
            }
          ];
          mode = "single";
          action = [
            {
              type = "turn_on";
              device_id = "4cf6a7217da6633d4a9c74fdf82ff30f";
              entity_id = "light.corridor_ceiling";
              domain = "light";
            }
          ];
        }
      ];
      tradfri = {};
      openweathermap = {};
      weather = {};
      scene = [
        {
          id = "1647007926507";
          name = "Abend";
          entities = {
            "switch.kette".state = "on";
            "switch.tradfri_outlet_2".state = "on"; # Kette 2
            "light.arod" = {
              min_mireds = 250;
              max_mireds = 454;
              color_mode = "hs";
              brightness = 76;
              hs_color = [29.021 74.588];
              state = "on";
            };
          };
        }
      ];
    };
  };

  virtualisation.oci-containers = {
    backend = "podman";
    containers.homeassistant = {
      volumes = ["home-assistant:/config"];
      environment.TZ = "Europe/Berlin";
      image = "ghcr.io/home-assistant/home-assistant:stable";
      extraOptions = [
        "--network=host"
        # "--device=/dev/ttyUSB0:/dev/ttyACM0"  # Example, change this to match your own hardware
      ];
    };
  };
}
