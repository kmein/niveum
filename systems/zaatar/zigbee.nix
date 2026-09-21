{
  config,
  pkgs,
  lib,
  ...
}:
let
  frontendPort = 8080;
  stateDirectory = "/var/lib/climate";

  climate-log = pkgs.writers.writePython3Bin "climate-log" { } ./climate-log.py;

  uplot =
    file: hash:
    pkgs.fetchurl {
      url = "https://cdn.jsdelivr.net/npm/uplot@1.6.32/dist/${file}";
      inherit hash;
    };

  webRoot = pkgs.linkFarm "climate-web" {
    "index.html" = ./climate.html;
    "uplot.js" = uplot "uPlot.iife.min.js" "sha256-GcjUxq2Ikpp59K5J1vcWFWbf0Lo9FcxJXpdPeH63jx8=";
    "uplot.css" = uplot "uPlot.min.css" "sha256-32MMao1vjur/JktQ9zzlsRT2Rv/ZoLt08EmwoAE1+gQ=";
  };
in
{
  imports = [ ../../configs/nginx.nix ];

  # only zigbee2mqtt and climate-log talk to it, both on this host
  services.mosquitto = {
    enable = true;
    listeners = [
      {
        address = "127.0.0.1";
        omitPasswordAuth = true;
        settings.allow_anonymous = true;
        acl = [ "topic readwrite #" ];
      }
    ];
  };

  services.zigbee2mqtt = {
    enable = true;
    settings = {
      serial = {
        port = "/dev/serial/by-id/usb-Texas_Instruments_TI_CC2531_USB_CDC___0X00124B0014DA44FB-if00";
        adapter = "zstack";
      };
      # must match the network stored on the stick, or zigbee2mqtt refuses to
      # start rather than orphan every paired device
      advanced = {
        pan_id = 6754;
        ext_pan_id = lib.genList (_: 221) 8;
        channel = 11;
        network_key = [
          1
          3
          5
          7
          9
          11
          13
          15
          0
          2
          4
          6
          8
          10
          12
          13
        ];
      };
      availability.enabled = true;
      frontend = {
        enabled = true;
        port = frontendPort;
      };
    };
  };

  # the frontend has no login, and retiolum is not a trusted network
  networking.firewall.interfaces.wlp2s0.allowedTCPPorts = [ frontendPort ];
  networking.firewall.allowedTCPPorts = [ 80 ];

  users.users.climate = {
    isSystemUser = true;
    group = "climate";
  };
  users.groups.climate = { };

  systemd.services.climate-log = {
    description = "Log zigbee climate sensor readings";
    wantedBy = [ "multi-user.target" ];
    wants = [ "mosquitto.service" ];
    after = [ "mosquitto.service" ];
    serviceConfig = pkgs.lib.niveum.hardening // {
      ExecStart = "${lib.getExe climate-log} ${lib.getExe' pkgs.mosquitto "mosquitto_sub"} -h 127.0.0.1 -t zigbee2mqtt/+ -F %%j";
      User = "climate";
      Group = "climate";
      StateDirectory = "climate";
      # nginx reads data.json
      StateDirectoryMode = "0755";
      UMask = "0022";
      Restart = "always";
      RestartSec = 10;
    };
  };

  services.nginx.virtualHosts.${config.networking.hostName + ".r"} = {
    default = true;
    root = webRoot;
    locations."= /data.json".alias = "${stateDirectory}/data.json";
  };

  services.restic.backups.niveum.paths = [
    stateDirectory
    config.services.zigbee2mqtt.dataDir
  ];
}
