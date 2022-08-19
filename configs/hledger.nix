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
      $EDITOR + "${timeLedger}" && ${pkgs.git}/bin/git -C "$(${pkgs.coreutils}/bin/dirname ${timeLedger})" commit --all --message "$(${pkgs.coreutils}/bin/date -Im)"
    '')
    (pkgs.writers.writeDashBin "hora" ''
      ${pkgs.hledger}/bin/hledger -f "${timeLedger}" "$@"
    '')
    (pkgs.writers.writeDashBin "hora-filli" ''
      ${pkgs.hledger}/bin/hledger -f "${timeLedger}" register fillidefilla -O csv \
        -b "$(date -d "$(date +%Y-%m)-20 last month" +%Y-%m-%d)" \
        -e "$(date -d "$(date +%Y-%m)-20" +%Y-%m-%d)" \
        | sed 's/(fillidefilla:\(.*\))/\1/g' \
        | xsv select date,amount,total,account,description
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
