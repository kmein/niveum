{ pkgs, config, lib, ... }:
with lib;
let cfg = config.niveum.google-drive;
in
{
  options.niveum.google-drive = {
    enable = mkEnableOption "Google Drive";
    directory = mkOption { type = types.path; };
    user = mkOption { type = types.attrs; };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.grive2
    ];

    systemd.services.google-drive = {
      description = "Google Drive synchronisation service";
      wantedBy = [ "network-online.target" ];
      script = ''
        ${pkgs.grive2}/bin/grive -p ${cfg.directory}
      '';
      startAt = "*:0/5";
      serviceConfig = {
        Type = "oneshot";
        User = cfg.user.name;
      };
    };
  };

}
