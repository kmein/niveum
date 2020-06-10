{ pkgs, lib, ... }: {
  imports = [ <niveum/modules/traadfri.nix> ];

  niveum.traadfri = {
    enable = true;
    user = "kmein";
    host = "192.168.178.28";
    key = lib.strings.fileContents <shared-secrets/traadfri.key>;
    rooms = {
      bedroom = 131082;
      corridor = 131080;
      kitchen = 131081;
      living-room = 131079;
    };
    bulbs.bedside = 65537;
  };
}
