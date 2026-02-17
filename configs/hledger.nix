{
  pkgs,
  ...
}:
let
  ledgerDirectory = "/home/kfm/sync/src/ledger";
in
{
  environment.systemPackages =
    let
      git = "${pkgs.git}/bin/git -C ${ledgerDirectory}";
    in
    [
      pkgs.hledger
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
