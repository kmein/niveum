{ pkgs, config, ... }:
let
  inherit (import <niveum/lib>) restic;
in
{
  services.restic.backups.niveum = {
    initialize = true;
    inherit (restic) repository;
    timerConfig = { OnCalendar = "00:05"; RandomizedDelaySec = "5h"; };
    passwordFile = toString <secrets/restic/password>;
    paths = [
      "/home/kfm/work"
    ];
  };

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "restic-niveum" ''
      ${pkgs.restic}/bin/restic -r ${restic.repository} -p ${<secrets/restic/password>} "$@"
    '')
  ];
}
