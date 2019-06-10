{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.niveum.traadfri;
  traadfri =
    let traadfri-package = pkgs.fetchFromGitHub {
      owner = "kmein";
      repo = "traadfri";
      rev = "9a34ce96363e0709adf9ff842e3dfc6d469e5217";
      sha256 = "1dj4xvzq51n2s3vnwh8f83lxn00x895wc92jp83x3pkcrjvkkzxn";
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
      (pkgs.writeShellScriptBin "traadfri" ''
        TRAADFRI_USER="${cfg.user}" \
        TRAADFRI_KEY="${cfg.key}" \
        TRAADFRI_HUB="${cfg.host}" \
        ${traadfri}/bin/traadfri $@
      '')
    ];
  };
}
