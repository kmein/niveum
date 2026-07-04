{
  services.music-assistant = {
    enable = true;
    providers = [
      "ard_audiothek"
      "bandcamp"
      "chromecast"
      "genius_lyrics"
      "gpodder"
      "hass_players"
      "radiobrowser"
      "radioparadise"
      "somafm"
      "soundcloud"
      "spotify"
      "spotify_connect"
      "tunein"
      "ytmusic"
    ];
  };

  networking.firewall.allowedTCPPorts = [
    8095
    8097
  ];
}
