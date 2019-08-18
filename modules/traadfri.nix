{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.niveum.traadfri;
  traadfri =
    let traadfri-package = pkgs.fetchFromGitHub {
      owner = "kmein";
      repo = "traadfri";
      rev = "652741b23213f6491ff3679f969716fdebde9bc3";
      sha256 = "0rvnc95yww4sc9qkg1pvdcgin3vx381fhx8kwcb6hlg30lpdhhrz";
    };
    in pkgs.python3Packages.callPackage traadfri-package {
      libcoap = pkgs.callPackage <packages/libcoap.nix> {};
    };
in
{
  options.niveum.traadfri = {
    enable = mkEnableOption "Trådfri CLI";
    user = mkOption { type = types.str; };
    host = mkOption { type = types.str; };
    key = mkOption { type = types.str; };
    rooms = mkOption { type = types.attrsOf types.int; default = {}; };
    bulbs = mkOption { type = types.attrsOf types.int; default = {}; };
  };

  config = mkIf cfg.enable {
    environment.shellAliases =
      lib.attrsets.mapAttrs'
        (name: value:
          lib.nameValuePair "traadfri-${name}" "traadfri bulb ${toString value}")
        cfg.bulbs
      // lib.attrsets.mapAttrs'
        (name: value:
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
