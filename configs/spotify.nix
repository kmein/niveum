{ pkgs, lib, ... }:
let
  inherit (lib.strings) fileContents;
in {
  environment.systemPackages = with pkgs; [ spotify spotify-tui playerctl ];

  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/audio/spotifyd.nix
  systemd.user.services.spotifyd = let
    spotifyd = pkgs.unstable.spotifyd.override {
      withMpris = true;
      withPulseAudio = true;
      inherit (pkgs) libpulseaudio dbus;
    };
    spotifydConf = pkgs.writeText "spotifyd.conf" (lib.generators.toINI { } {
      global = {
        username = fileContents <shared-secrets/spotify/username>;
        password = fileContents <shared-secrets/spotify/password>;
        backend = "pulseaudio";
        on_song_change_hook = toString (pkgs.writers.writeDash "songinfo" ''
          PATH=$PATH:${
            lib.makeBinPath [ pkgs.playerctl pkgs.gawk pkgs.libnotify ]
          }
          metadata=$(playerctl metadata --player spotifyd)
          title=$(echo "$metadata" | awk '/^xesam:title\s/ { print substr($0, index($0, $3)) }')
          artist=$(echo "$metadata" | awk '/^xesam:artist\s/ { print substr($0, index($0, $3)) }' | paste --serial --delimiters "/")
          album=$(echo "$metadata" | awk '/^xesam:album\s/ { print substr($0, index($0, $3)) }')
          notify-send --app-name=" Spotify" "$title" "$artist – $album"
        '');
      };
    });
  in {
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" "sound.target" ];
    description = "spotifyd, a Spotify playing daemon";
    serviceConfig = {
      ExecStart =
        "${spotifyd}/bin/spotifyd --no-daemon --config-path ${spotifydConf}";
      Restart = "always";
      RestartSec = 12;
    };
  };
}
