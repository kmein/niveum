{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.niveum.hledger;
in {
  options.niveum.hledger = {
    enable = mkEnableOption "hledger";
    server = {
      enable = mkEnableOption "hledger server";
      port = mkOption { type = pkgs.unstable.lib.types.port; default = 5000; };
      host = mkOption { type = types.str; default = "127.0.0.1"; };
      capabilities = mkOption {
        type = types.listOf (types.enum ["view" "add" "manage"]);
        default = [ "view" "add" ];
      };
      flags = mkOption { type = types.listOf types.str; default = []; };
      user = mkOption { type = types.attrs; };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.unstable.hledger ];

    systemd.services.hledger-web = mkIf cfg.server.enable {
      description = "hledger server";
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Restart = "always";
        ExecStart = ''
          ${pkgs.unstable.hledger-web}/bin/hledger-web \
            --port=${toString cfg.server.port} \
            --host=${cfg.server.host} \
            --capabilities=${concatStringsSep "," cfg.server.capabilities} \
            ${concatStringsSep " " cfg.server.flags}
        '';
        User = cfg.server.user.name;
        PrivateTemp = true;
        RuntimeDirectory = "hledger-web";
        WorkingDirectory = "%t/hledger-web";
      };
    };
  };
}
