{ pkgs, ... }:
let
  autowifi = pkgs.writers.writePython3Bin "autowifi" { flakeIgnore = [ "E501" ]; } <stockholm/lass/5pkgs/autowifi/autowifi.py>;
in
{
  networking.networkmanager = {
    enable = true;
    wifi.macAddress = "random";
    ethernet.macAddress = "random";
    unmanaged = [ "docker*" ];
  };

  users.users.me.extraGroups = [ "networkmanager" ];

  systemd.services.autowifi = {
    description = "Automatic wifi connector";
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.networkmanager ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = "10s";
      ExecStart = "${autowifi}/bin/autowifi";
    };
  };

  networking.networkmanager.dispatcherScripts = [
    { source = "${(pkgs.callPackage <stockholm/makefu/5pkgs/prison-break> {})}/bin/prison-break"; }
  ];
}
