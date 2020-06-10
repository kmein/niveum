{ pkgs, config, lib, ... }:
with lib;
let cfg = config.niveum.google-drive;
in {
  options.niveum.google-drive = {
    enable = mkEnableOption "Google Drive";
    directory = mkOption { type = types.path; };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.grive2 ];

    systemd.user.services.google-drive = {
      description = "Google Drive synchronisation service";
      after = [ "network.target" ];
      wantedBy = [ "default.target" ];
      script = "${pkgs.grive2}/bin/grive -p ${cfg.directory}";
      startAt = "*:0/5";
      serviceConfig.Type = "oneshot";
    };
  };

}
