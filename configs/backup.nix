{
  services.restic.backups.niveum = {
    initialize = true;
    repository = "rest:http://zaatar.r:3571/";
    timerConfig = { OnCalendar = "00:05"; RandomizedDelaySec = "5h"; };
    passwordFile = toString <secrets/restic/password>;
    paths = [
      "/home/kfm/work"
    ];
  };
}
