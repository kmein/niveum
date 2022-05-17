{
  config,
  pkgs,
  ...
}: {
  imports = [<niveum/modules/hledger.nix>];

  environment.systemPackages = let
    timeLedger = "$HOME/projects/ledger/time.timeclock";
  in [
    (pkgs.writers.writeDashBin "hora-edit" ''
      $EDITOR + "${timeLedger}"
    '')
    (pkgs.writers.writeDashBin "hora" ''
      ${pkgs.hledger}/bin/hledger -f "${timeLedger}" "$@"
    '')
  ];

  niveum.hledger = {
    enable = true;
    ledgerFile = "$HOME/projects/ledger/all.journal";
    server = {
      enable = false;
      user = config.users.users.me;
      package = pkgs.hledger-web;
    };
    package = pkgs.hledger;
  };
}
