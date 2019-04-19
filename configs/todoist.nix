{ pkgs, ... }:
let secrets = import <dot/secrets.nix>;
in {
  environment.systemPackages = [
    (pkgs.unstable.writers.writeDashBin "todoist" ''
      ${pkgs.todoist}/bin/todoist --color $@
    '')
  ];

  home-manager.users.me.home.file.".todoist.config.json".text = ''
    {
      "token": "${secrets.todoist.token}"
    }
  '';
}
