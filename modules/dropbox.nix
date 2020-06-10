{ pkgs, lib, config, ... }:
with lib;
let cfg = config.niveum.dropbox;
in {
  options.niveum.dropbox = { enable = mkEnableOption "Dropbox"; };

  config = mkIf cfg.enable {
    systemd.user.services.dropbox = {
      description = "Dropbox synchronisation service";
      after = [ "network.target" ];
      wantedBy = [ "default.target" ];
      path = [ pkgs.dropbox-cli ];
      serviceConfig = {
        Type = "forking";
        PIDFile = "%h/.dropbox/dropbox.pid";
        Restart = "always";
        ExecStart = "${pkgs.dropbox-cli}/bin/dropbox start";
        ExecStop = "${pkgs.dropbox-cli}/bin/dropbox stop";
      };
    };
  };
}
