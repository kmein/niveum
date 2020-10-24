{ pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) localAddresses;
  living-room-id = 131086;
in
{
  imports = [ <niveum/modules/traadfri.nix> ];

  niveum.traadfri = {
    enable = true;
    user = "kmein";
    host = localAddresses.tradfri;
    key = lib.strings.fileContents <secrets/traadfri.key>;
    rooms = {
      corridor = 131080;
      kitchen = 131081;
      bedroom = 131082;
      living-room = 131086;
      bedside = 131087;
    };
  };
}
