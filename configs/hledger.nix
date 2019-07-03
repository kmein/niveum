{ config, pkgs, ... }:
{
  imports = [ <modules/hledger.nix> ];

  niveum.hledger = {
    enable = true;
    ledgerFile = "$HOME/prog/git/ledger/current.journal";
    server = {
      enable = false;
      user = config.users.users.me;
      package = pkgs.unstable.hledger-web;
    };
    package = pkgs.unstable.hledger;
  };
}
