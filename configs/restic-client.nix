{ pkgs, config, ... }:
{
  age.secrets.restic.file = ../secrets/restic.age;

  services.restic.backups.niveum = {
    initialize = true;
    repository = pkgs.lib.niveum.restic.repository;
    timerConfig = {
      OnCalendar = "daily";
      RandomizedDelaySec = "1h";
    };
    passwordFile = config.age.secrets.restic.path;
  };
}
