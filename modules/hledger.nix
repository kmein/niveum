{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.niveum.hledger;
  hledger-git = pkgs.writers.writeDashBin "hledger-git" ''
    GIT="${pkgs.git}/bin/git -C $(dirname $LEDGER_FILE)"
    if [ "$1" = entry ]; then
      $GIT commit --all --message="$(date +%F)"
    else
      $GIT $*
    fi
  '';
in {
  options.niveum.hledger = {
    enable = mkEnableOption "hledger";
    package = mkOption { type = types.package; default = pkgs.hledger; };
    ledgerFile = mkOption { type = types.str; default = null; };
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
      package = mkOption { type = types.package; default = pkgs.hledger-web; };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package hledger-git ];

    environment.variables.LEDGER_FILE = mkIf (cfg.ledgerFile != null) cfg.ledgerFile;

    systemd.services.hledger-web = mkIf cfg.server.enable {
      description = "hledger server";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Restart = "always";
        ExecStart = ''
          ${cfg.server.package}/bin/hledger-web \
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
