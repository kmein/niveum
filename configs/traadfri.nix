{ pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) localAddresses;
  living-room-id = 131086;
in
{
  imports = [ <niveum/modules/traadfri.nix> ];

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "traadfri-party" ''
      while true; do
        for color in $(traadfri colours | shuf); do
          echo "$color"
          traadfri group "''${2:-${toString living-room-id}}" --on --colour="$color"
          sleep "''${1:-2}"
        done
      done
    '')
  ];

  niveum.traadfri = {
    enable = true;
    user = "kmein";
    host = localAddresses.tradfri;
    key = lib.strings.fileContents <secrets/traadfri.key>;
    rooms = {
      corridor = 131080;
      kitchen = 131081;
      bedroom = 131082;
      living-room = living-room-id;
      bedside = 131087;
      desk = 131089;
    };
  };
}
