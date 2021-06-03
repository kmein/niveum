{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.niveum.traadfri;
  traadfri = let
    traadfri-package = pkgs.fetchFromGitHub {
      owner = "kmein";
      repo = "traadfri";
      rev = "6385d5b9e79910b4caee876536176cf9d6a8b9cc";
      sha256 = "1l0vsxf4f4k9hgqa8sjkq4pgidbhpwps8lr3dvvnwnjh5636pbs9";
    };
  in pkgs.python3Packages.callPackage traadfri-package {
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
      lib.nameValuePair "traadfri-${name}" "traadfri bulb ${toString value}")
      cfg.bulbs // lib.attrsets.mapAttrs' (name: value:
        lib.nameValuePair "traadfri-${name}" "traadfri group ${toString value}")
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
