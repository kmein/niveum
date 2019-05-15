{ pkgs, ... }:
let secrets = import <dot/secrets.nix>;
in {
  environment.systemPackages = [ pkgs.python3Packages.sncli ];

  home-manager.users.me = {
    home.file.".snclirc".text = ''
      [sncli]
      cfg_sn_username = ${secrets.simplenote.username}
      cfg_sn_password = ${secrets.simplenote.password}
    '';
  };
}
