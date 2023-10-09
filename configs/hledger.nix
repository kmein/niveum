{
  config,
  pkgs,
  ...
}: let
  ledgerDirectory = "$HOME/projects/ledger";
  timeLedger = "${ledgerDirectory}/time.timeclock";
  hora = pkgs.writers.writeDashBin "hora" ''
    ${pkgs.hledger}/bin/hledger -f "${timeLedger}" "$@"
  '';
in {
  environment.systemPackages = let
    git = "${pkgs.git}/bin/git -C ${ledgerDirectory}";
  in [
    pkgs.hledger
    (pkgs.writers.writeDashBin "hora-edit" ''
      $EDITOR + "${timeLedger}" && ${pkgs.git}/bin/git -C "$(${pkgs.coreutils}/bin/dirname ${timeLedger})" commit --all --message "$(${pkgs.coreutils}/bin/date -Im)"
    '')
    hora
    (pkgs.writers.writeDashBin "hora-year" ''
      ${hora}/bin/hora bal --tree --monthly --begin $(date +%Y) --depth 1
    '')
    (pkgs.writers.writeDashBin "hora-filli" ''
      ${pkgs.hledger}/bin/hledger -f "${timeLedger}" register fillidefilla -O csv \
        -b "$(date -d "$(date +%Y-%m)-01 last month" +%Y-%m-%d)" \
        -e "$(date -d "$(date +%Y-%m)-01" +%Y-%m-%d)" \
        | sed 's/(fillidefilla:\(.*\))/\1/g' \
        | xsv select date,amount,total,account,description
    '')

    (pkgs.writers.writeDashBin "hledger-git" ''
      if [ "$1" = entry ]; then
        ${pkgs.hledger}/bin/hledger balance -V > "${ledgerDirectory}/balance.txt"
        ${git} add balance.txt
        ${git} commit --all --message="$(date -Im)"
      else
        ${git} $*
      fi
    '')
    (pkgs.writers.writeDashBin "hledger-edit" ''
      $EDITOR ${ledgerDirectory}/current.journal
    '')
  ];
}
