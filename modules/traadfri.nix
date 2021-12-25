{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.niveum.traadfri;
  traadfri = let
    traadfri-package = pkgs.fetchFromGitHub {
      owner = "kmein";
      repo = "traadfri";
      rev = "cf46bd09cd3263b90a09b0ca979aa705a4c3671c";
      sha256 = "0az9q38pl8fqk00488blhn6rhvwsw2wky3dbdlyz7945ggvxnbyd";
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
