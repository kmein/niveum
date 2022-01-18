{ pkgs, config, ... }:
let
  repository = "rest:http://zaatar.r:3571/";
in
{
  services.restic.backups.niveum = {
    initialize = true;
    inherit repository;
    timerConfig = { OnCalendar = "00:05"; RandomizedDelaySec = "5h"; };
    passwordFile = toString <secrets/restic/password>;
    paths = [
      "/home/kfm/work"
    ];
  };

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "restic-niveum" ''
      ${pkgs.restic}/bin/restic -r ${repository} -p ${<secrets/restic/password>} "$@"
    '')
  ];
}
