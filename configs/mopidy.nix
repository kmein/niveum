{ pkgs, ... }:
let secrets = import <dot/secrets.nix>;
in {
  services.mopidy = {
    enable = true;
    extensionPackages = [
      pkgs.mopidy-gmusic
      pkgs.mopidy-iris
      pkgs.mopidy-moped
      pkgs.mopidy-mopify
      pkgs.mopidy-soundcloud
      pkgs.mopidy-spotify
      pkgs.mopidy-spotify-tunigo
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
