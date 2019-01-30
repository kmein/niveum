{ config, pkgs, ... }:
{
  systemd.services.google-drive = {
    description = "Google Drive synchronisation service";
    wants = [ "network-online.target" ];
    script = ''
      ${pkgs.grive2}/bin/grive -p ${config.users.users.kfm.home}/cloud/gdrive
    '';
    startAt = "*:0/5";
    serviceConfig = {
      Restart = "on-failure";
      User = "kfm";
    };
  };
}
