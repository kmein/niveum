{ pkgs, lib, ... }:
{
  imports = [ <niveum/modules/traadfri.nix> ];

  niveum.traadfri = {
    enable = true;
    user = "kmein";
    host = "192.168.178.28";
    key = lib.strings.fileContents <shared-secrets/traadfri.key>;
    rooms = {
      bedroom = 131074;
      corridor = 131076;
      kitchen = 131075;
      living-room = 131073;
    };
    bulbs = {
      bedread = 65546;
      livingread = 65537;
    };
  };
}
