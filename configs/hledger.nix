{ config, ... }:
{
  imports = [ <modules/hledger.nix> ];

  niveum.hledger = {
    enable = true;
    server = {
      enable = true;
      user = config.users.users.me;
    };
  };
}
