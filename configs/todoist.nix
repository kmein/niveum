{ pkgs, ... }:
let
  secrets = import <dot/secrets.nix>;
  todoist = pkgs.unstable.callPackage <packages/todoist.nix> {};
in {
  environment.systemPackages = [
    (pkgs.unstable.writers.writeDashBin "todoist" ''
      ${todoist}/bin/todoist --color $@
    '')
  ];

  home-manager.users.me.home.file.".todoist.config.json".text = ''
    {
      "token": "${secrets.todoist.token}"
    }
  '';
}
