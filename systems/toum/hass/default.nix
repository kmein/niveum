{ config, pkgs, lib, ... }:
let
  inherit (import ./lib.nix) triggers;
  inherit (import <niveum/lib>) localAddresses;
in
{
  disabledModules = [
    "services/misc/home-assistant.nix"
  ];

  imports = [
    ./zigbee.nix
    ./frontend.nix
    <nixpkgs-unstable/nixos/modules/services/misc/home-assistant.nix>
  ];

  services.home-assistant = {
    enable = true;
    configWritable = true;
    lovelaceConfigWritable = true;
    openFirewall = true;
    config = {
      homeassistant = {
        name = "Toum";
        latitude = 52.461;
        longitude = 13.378;
        elevation = 90; # TODO find out how high I live
        unit_system = "metric";
        time_zone = config.time.timeZone;
      };
      config = {};
      discovery = {};
      system_health = {};
      history = {};
      # tradfri.host = localAddresses.tradfri; # dont use until python3Packages.pytradfri is packaged
      sun = {};
      mobile_app = {};
      shopping_list = {};
      sensor = [
        {
          platform = "dwd_weather_warnings";
          region_name = "Berlin";
        }
      ];
      mqtt = {
        broker = "localhost";
        port = 1883;
        client_id = "home-assistant";
        username = "albrecht";
        password = lib.strings.fileContents <system-secrets/mosquitto>;
        keepalive = 60;
        protocol = "3.1";

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
}
