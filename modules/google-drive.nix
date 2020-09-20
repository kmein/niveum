{ pkgs, config, lib, ... }:
with lib;
let cfg = config.niveum.google-drive;
in {
  options.niveum.google-drive = {
    enable = mkEnableOption "Google Drive";
    directory = mkOption { type = types.path; };
  };

  config = mkIf cfg.enable {
    systemd.user.services.google-drive = {
      description = "Google Drive synchronisation service";
      after = [ "network.target" ];
      wantedBy = [ "default.target" ];
      preStart = "mkdir -p ${cfg.directory}";
      script = "${pkgs.google-drive-ocamlfuse}/bin/google-drive-ocamlfuse ${cfg.directory}";
      preStop = "${pkgs.fuse}/bin/fusermount -u ${cfg.directory}";
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
    };
  };

}
