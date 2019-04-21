{ config, pkgs, ... }:
{
  imports = [ <modules/hledger.nix> ];

  niveum.hledger = {
    enable = true;
    server = {
      enable = true;
      user = config.users.users.me;
      package = pkgs.unstable.hledger-web;
    };
    package = pkgs.unstable.hledger;
  };
}
