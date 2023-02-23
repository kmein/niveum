{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
with lib; let
  cfg = config.niveum.traadfri;
in {
  options.niveum.traadfri = {
    enable = mkEnableOption "Trådfri CLI";
    user = mkOption {type = types.str;};
    host = mkOption {type = types.str;};
    keyFile = mkOption {type = types.path;};
    rooms = mkOption {
      type = types.attrsOf types.int;
      default = {};
    };
    bulbs = mkOption {
      type = types.attrsOf types.int;
      default = {};
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages =
      [
        (pkgs.writers.writeDashBin "traadfri" ''
          export TRAADFRI_USER="${cfg.user}"
          export TRAADFRI_KEY="$(cat ${lib.escapeShellArg cfg.keyFile})"
          export TRAADFRI_HUB="${cfg.host}"
          ${inputs.traadfri.defaultPackage.x86_64-linux}/bin/traadfri $@
        '')
      ]
      ++ lib.mapAttrsToList (name: value:
        pkgs.writers.writeDashBin "traadfri-${name}" ''
          exec traadfri --target Room ${toString value} "$@"
        '')
      cfg.rooms
      ++ lib.mapAttrsToList (name: value:
        pkgs.writers.writeDashBin "traadfri-${name}" ''
          exec traadfri --target Bulb ${toString value} "$@"
        '')
      cfg.bulbs;
  };
}
