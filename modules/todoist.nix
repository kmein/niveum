{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.niveum.todoist;
  todoist = pkgs.unstable.callPackage <packages/todoist.nix> {};
in {
  options.niveum.todoist = {
    enable = mkEnableOption "todoist CLI";
    token = mkOption { type = types.strMatching "[0-9a-f]+"; };
    color = mkOption { type = types.bool; default = true; };
  };

  config = {
    environment.systemPackages = mkIf cfg.enable [
      (pkgs.writers.writeDashBin "todoist" ''
        ${todoist}/bin/todoist $@
      '')
    ];

    home-manager.users.me.home.file.".todoist.config.json".text = mkIf cfg.enable (builtins.toJSON {
      token = cfg.token;
      color = cfg.color;
    });

    systemd.user.services.todoist-sync = {
      enable = cfg.enable;
      after = [ "network-online.target" ];
      startAt = "*:0/5";
      script = ''${todoist}/bin/todoist sync'';
    };
  };
}
