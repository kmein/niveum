{ pkgs, lib, config, ... }:
with lib;
let cfg = config.niveum.dropbox;
in
{
  options.niveum.dropbox = {
    enable = mkEnableOption "Dropbox";
  };

  config = mkIf cfg.enable {
    services.xserver.displayManager.sessionCommands = "${pkgs.dropbox}/bin/dropbox &";

    environment.systemPackages = [ pkgs.dropbox-cli ];
  };
}
