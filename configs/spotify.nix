{ pkgs, lib, ... }:
let
  inherit (import <niveum/lib> { inherit pkgs; }) toTOML;
  inherit (lib.strings) fileContents;
in
{
  environment.systemPackages = with pkgs; [
    spotify
    spotify-tui
  ];

  services.spotifyd = {
    enable = true;
    config = toTOML {
      global = {
        username = fileContents <shared-secrets/spotify/username>;
        password_cmd = "${pkgs.pass}/bin/pass shared/spotify/password";
      };
    };
  };
}
