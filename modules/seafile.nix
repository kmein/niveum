{ pkgs, config, lib, ... }:
with lib;
let cfg = config.niveum.seafile;
in {
  options.niveum.seafile = {
    enable = mkEnableOption "Seafile";
  };

  config = lib.mkIf cfg.enable {
    services.xserver.displayManager.sessionCommands = "${pkgs.seafile-client}/bin/seafile-applet &";

    environment.systemPackages = [ pkgs.seafile-client pkgs.seafile-shared ];
  };
}
