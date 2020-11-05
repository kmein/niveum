{ pkgs, lib, config, ... }:
with lib;
let cfg = config.niveum.dropbox;
in {
  options.niveum.dropbox = { enable = mkEnableOption "Dropbox"; };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.dropbox-cli ];

    networking.firewall = {
      allowedTCPPorts = [ 17500 ];
      allowedUDPPorts = [ 17500 ];
    };

    systemd.user.services.dropbox = {
      description = "Dropbox synchronisation service";
      wantedBy = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
        ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
        KillMode = "control-group"; # upstream recommends process
        Restart = "on-failure";
        PrivateTmp = true;
        ProtectSystem = "full";
        Nice = 10;
      };
    };
  };
}
