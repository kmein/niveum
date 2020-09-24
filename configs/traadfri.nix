{ pkgs, lib, ... }: {
  imports = [ <niveum/modules/traadfri.nix> ];

  niveum.traadfri = {
    enable = true;
    user = "kmein";
    host = "192.168.178.28";
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
