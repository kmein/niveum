{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.niveum.traadfri;
  traadfri = pkgs.callPackage <traadfri> {
    libcoap = pkgs.callPackage <niveum/packages/libcoap.nix> {tls = true;};
  };
in {
  options.niveum.traadfri = {
    enable = mkEnableOption "Trådfri CLI";
    user = mkOption {type = types.str;};
    host = mkOption {type = types.str;};
    key = mkOption {type = types.str;};
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
          TRAADFRI_USER="${cfg.user}" \
          TRAADFRI_KEY="${cfg.key}" \
          TRAADFRI_HUB="${cfg.host}" \
          ${traadfri}/bin/traadfri $@
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
