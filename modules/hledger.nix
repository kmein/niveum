{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.niveum.hledger;
  hledger-git = pkgs.writers.writeDashBin "hledger-git" ''
    LEDGER_DIR="$(dirname $LEDGER_FILE)"
    GIT="${pkgs.git}/bin/git -C ''${LEDGER_DIR}"
    if [ "$1" = entry ]; then
      ${cfg.package}/bin/hledger balance -V > "$LEDGER_DIR/balance.txt"
      $GIT add balance.txt
      $GIT commit --all --message="$(date -Im)"
    else
      $GIT $*
    fi
  '';
  hledger-edit = pkgs.writers.writeDashBin "hledger-edit" ''
    LEDGER_DIR="$(dirname $LEDGER_FILE)"
    $EDITOR ''${LEDGER_DIR}/current.journal
  '';
in {
  options.services.hledger = {
    enable = mkEnableOption "hledger";
    ledgerFile = mkOption {
      type = types.str;
    };
    server = {
      enable = mkEnableOption "hledger server";
      port = mkOption {
        type = types.port;
        default = 5000;
      };
      host = mkOption {
        type = types.str;
        default = "127.0.0.1";
      };
      capabilities = mkOption {
        type = types.listOf (types.enum ["view" "add" "manage"]);
        default = ["view" "add"];
      };
      flags = mkOption {
        type = types.listOf types.str;
        default = [];
      };
      user = mkOption {type = types.attrs;};
      package = mkOption {
        type = types.package;
        default = pkgs.hledger-web;
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.hledger-web = mkIf cfg.server.enable {
      description = "hledger server";
      wantedBy = ["multi-user.target"];
      environment.LEDGER_FILE = cfg.ledgerFile;
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
