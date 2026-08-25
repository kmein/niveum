{
  config,
  pkgs,
  lib,
  ...
}:
let
  # Radio stations for Music Assistant's builtin provider, generated from the
  # curated stream list. MA keeps manually added radio stations in its
  # settings.json under `stored_radios`; they show up in the library at the
  # next builtin provider sync.
  stored-radios = pkgs.writeText "music-assistant-radios.json" (
    builtins.toJSON (
      map (
        stream:
        {
          item_id = stream.stream;
          name = stream.station;
        }
        // lib.optionalAttrs (stream ? logo && lib.hasPrefix "http" stream.logo) {
          image_url = stream.logo;
        }
      ) pkgs.radio-streams.streams
    )
  );
in
{
  services.music-assistant = {
    enable = true;
    # MA hard-rejects logging in to gpodder.net (the provider deems the site
    # too unreliable); neuter the check and let it try anyway. Also gpodder.net
    # 403s any User-Agent containing "Python/x.y" (MA's session default), so
    # give the gpodder client its own.
    package = pkgs.music-assistant.overrideAttrs (old: {
      postPatch = (old.postPatch or "") + ''
        substituteInPlace music_assistant/providers/gpodder/__init__.py \
          --replace-fail 'if base_url.rstrip("/") == "https://gpodder.net":' 'if False:'
        substituteInPlace music_assistant/providers/gpodder/client.py \
          --replace-fail \
          'headers=self.headers if self.is_nextcloud else None,' \
          'headers=self.headers if self.is_nextcloud else {"User-Agent": "MusicAssistant"},'
        # gpodder.net history contains play actions with null position/started/total,
        # which the EpisodeActionPlay model (plain int fields) refuses -> whole
        # podcast sync dies. Normalize nulls to 0 before deserializing.
        substituteInPlace music_assistant/providers/gpodder/client.py \
          --replace-fail \
          'actions_response = EpisodeActionGet.from_json(response)' \
          '_data = __import__("json").loads(response); _data["actions"] = [{**_a, **{_k: 0 for _k in ("started", "position", "total") if _a.get(_k, 0) is None}} for _a in _data.get("actions", [])]; actions_response = EpisodeActionGet.from_dict(_data)'
      '';
    });
    extraOptions = [
      # keep the module default data dir when overriding this option
      "--config"
      "/var/lib/music-assistant"
      # for debugging the gpodder login
      "--log-level"
      "DEBUG"
    ];
    providers = [
      "ard_audiothek"
      "bandcamp"
      "chromecast"
      "genius_lyrics"
      "gpodder"
      "hass_players"
      "radiobrowser"
      "radioparadise"
      # sendspin is a `builtin: true` provider, so it's always loaded.
      # Listing it here pulls aiosendspin+av — without them, chromecast's
      # unconditional `from .sendspin_bridge import …` also fails.
      "sendspin"
      "somafm"
      "soundcloud"
      "spotify"
      "spotify_connect"
      "tunein"
    ];
  };

  systemd.services.music-assistant = {
    # di-fm-key is declared in home-assistant.nix; LoadCredential makes it
    # readable despite DynamicUser.
    serviceConfig.LoadCredential = [ "di-fm-key:${config.age.secrets.di-fm-key.path}" ];
    # Replace MA's manually-added radio stations with the generated list on
    # every start; the Nix list is authoritative, so stations added via the
    # UI do not survive a restart (add them to packages/streams instead).
    preStart = ''
      settings="$STATE_DIRECTORY/settings.json"
      [ -s "$settings" ] || echo '{}' > "$settings"
      ${pkgs.gnused}/bin/sed "s|%DI_FM_KEY%|$(cat "$CREDENTIALS_DIRECTORY/di-fm-key")|g" ${stored-radios} \
        | ${pkgs.jq}/bin/jq --slurpfile settings "$settings" '
            ($settings[0] // {}) + {stored_radios: unique_by(.item_id)}
          ' > "$settings.tmp"
      mv "$settings.tmp" "$settings"
    '';
  };

  # settings.json holds stateful config like the streamserver publish IP,
  # library.db the music library.
  services.restic.backups.niveum.paths = [ "/var/lib/music-assistant" ];

  networking.firewall.allowedTCPPorts = [
    8095
    8097
  ];
}
