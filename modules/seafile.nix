{ pkgs, config, lib, ... }:
with lib;
let cfg = config.niveum.seafile;
in {
  options.niveum.seafile = {
    enable = mkEnableOption "Seafile";
  };

  config = mkIf cfg.enable {
    systemd.user.services.seafile = {
      description = "Seafile synchronisation service";
      after = [ "network.target" ];
      wantedBy = [ "default.target" ];
      script = "${pkgs.seafile-client}/bin/seafile-applet";
      serviceConfig = {
        Restart = "always";
      };
    };

    environment.systemPackages = [ pkgs.seafile-client pkgs.seafile-shared ];
  };
}
