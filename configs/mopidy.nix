{ pkgs, ... }:
let secrets = import <dot/secrets.nix>;
in {
  services.mopidy = {
    enable = true;
    extensionPackages = [
      pkgs.mopidy-spotify
      pkgs.mopidy-iris
      pkgs.mopidy-gmusic
      pkgs.mopidy-moped
      pkgs.mopidy-soundcloud
      pkgs.mopidy-youtube
    ];
    configuration = ''
      [mpd]
      hostname = ::

      [spotify]
      username = ${secrets.spotify.username}
      password = ${secrets.spotify.password}
      client_id = ${secrets.spotify.clientId}
      client_secret = ${secrets.spotify.clientSecret}

      [soundcloud]
      auth_token = ${secrets.soundcloud.authToken}
    '';
  };
}
