{
  config,
  pkgs,
  ...
}: {
  imports = [<niveum/modules/hledger.nix>];

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
