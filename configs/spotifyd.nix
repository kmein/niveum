{ lib, ... }:
{
  services.spotifyd = {
    enable = true;
    config = lib.generators.toINI { } {
      global = {
        username = lib.strings.fileContents <secrets/spotify/username>;
        password = lib.strings.fileContents <secrets/spotify/password>;
      };
    };
  };
}
