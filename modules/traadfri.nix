{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.niveum.traadfri;
  traadfri = let
    traadfri-package = pkgs.fetchFromGitHub {
      owner = "kmein";
      repo = "traadfri";
      rev = "a55b778cf4c7f124e75070b056c90f5a8043bc45";
      sha256 = "10ggqksdbgmcwc2f3xs6abjf9zpvv554xfl3b3bblmfijgr6srv9";
    };
  in pkgs.callPackage traadfri-package {
    libcoap = pkgs.callPackage <niveum/packages/libcoap.nix> { tls = true; };
  };
in {
  options.niveum.traadfri = {
    enable = mkEnableOption "Trådfri CLI";
    user = mkOption { type = types.str; };
    host = mkOption { type = types.str; };
    key = mkOption { type = types.str; };
    rooms = mkOption {
      type = types.attrsOf types.int;
      default = { };
    };
    bulbs = mkOption {
      type = types.attrsOf types.int;
      default = { };
    };
  };

  config = mkIf cfg.enable {
    environment.shellAliases = lib.attrsets.mapAttrs' (name: value:
      lib.nameValuePair "traadfri-${name}" "traadfri --target Bulb ${toString value}")
      cfg.bulbs // lib.attrsets.mapAttrs' (name: value:
        lib.nameValuePair "traadfri-${name}" "traadfri --target Room ${toString value}")
      cfg.rooms;

    environment.systemPackages = [
      (pkgs.writers.writeDashBin "traadfri" ''
        TRAADFRI_USER="${cfg.user}" \
        TRAADFRI_KEY="${cfg.key}" \
        TRAADFRI_HUB="${cfg.host}" \
        ${traadfri}/bin/traadfri $@
      '')
    ];
  };
}
