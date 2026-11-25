{
  config,
  lib,
  pkgs,
  ...
}:
let
  port = 10200;

  # piper is calibre's read-aloud engine too; voices are listed at
  # https://huggingface.co/rhasspy/piper-voices
  voices = {
    de_DE-karlsson-low = {
      language = "de";
      type = "MALE1";
      model = "sha256-+Xk79+RhpHKzkWzyYHdL/JgS6etyYBsUftZJ0BBk6P4=";
      config = "sha256-OyCVrb/u3jFvIgB6CgnUv8JVFLcYmFB0CwaACmz7mDg=";
    };
    en_GB-jenny_dioco-medium = {
      language = "en";
      type = "FEMALE1";
      model = "sha256-YroYOlBM/NiBbsG1DuhW7TGMzJ3SL9C+3X2DnJXfog4=";
      config = "sha256-LSiAQUXjfPayd9F0F3aaLIT25BVyc2eN/klSQMxuAOk=";
    };
    ar_JO-kareem-medium = {
      language = "ar";
      type = "MALE1";
      model = "sha256-npXKsHtnnaYDu6F8Tex6sxETIFcZZO6VwDeWA8CGSR4=";
      config = "sha256-6m2bnZB229tr9cmMahQe8VSVnSNZcJs3hVcnlk59bE0=";
    };
    he_IL-saspeech-medium = {
      language = "he";
      type = "MALE1";
      model = "sha256-RaX+H03AJ/3TnTos6AJCWA8jSuYxm26MZ/aWbkzE/iM=";
      config = "sha256-iJDC9J+d64Lsi8j0pN82q+sPwxODvmOLuwl37ZZFfL4=";
    };
    hi_IN-priyamvada-medium = {
      language = "hi";
      type = "FEMALE1";
      model = "sha256-qmO88s1JO1WkUPKA4jz3fwOvya9wFeblrNQ7ZS8WbIg=";
      config = "sha256-XvwMz3Up81KJltRuD6wflp9oHUSo5Vv6Yjb/iEG11S0=";
    };
    ur_PK-aegis_female-medium = {
      language = "ur";
      type = "FEMALE1";
      model = "sha256-PsMGA1z9C7wiNC/uWJBFBwihbdIxgJUicxZ4qlg8lh8=";
      config = "sha256-qDhcIXp+J2rPUjS/AdvQrbAHpXwy/7zFfS236sOXm7Y=";
    };
    pl_PL-gosia-medium = {
      language = "pl";
      type = "FEMALE1";
      model = "sha256-0yLe0fHa/+q0G7rNQ9b2Ksg6Sl1NY6KXqjnzRfUGs1g=";
      config = "sha256-8MwvT1PWAnSnm/bsnY0XwC4YiuPHzBcPbTCKl1JsNRs=";
    };
    # rapunzelina stretches and clips greek words at random
    el_GR-joy-medium = {
      language = "el";
      type = "FEMALE1";
      model = "sha256-MbHVHcct1DpUKbZ3mOJ1mfjNz86KgCZbxvUNkyvxRLw=";
      config = "sha256-RRqIhaDqcuRDL//iLmWnW70uZfZ1cnPRpantijQZhS4=";
    };
  };

  # piper has no voice for these, so anki reads them with espeak-ng
  espeakLocales = [ "ta_IN" ];

  # by top-level deck: the language of a bare <tts>, for which ankidroid asks per deck, and the fields
  # to read where the templates mark nothing, since marking them would reach every deck sharing the note type.
  # the levantine deck is romanised and coptic has no voice, so they stay silent
  ankiDecks = {
    "4 🇮🇳 — தமிழ்".language = "ta_IN";
    "🇬🇷 — ἑλληνική" = {
      language = "el_GR";
      fields = [
        "Original"
        "Example (Original)"
      ];
    };
    # no voice speaks sanskrit; hindi's drops the final a
    "🇮🇳 — संस्कृतम्" = {
      language = "hi_IN";
      fields = [
        "Original"
        "Example (Original)"
      ];
    };
  };

  voiceDir = pkgs.linkFarm "piper-voices" (
    lib.concatLists (
      lib.mapAttrsToList (
        name: voice:
        let
          # <locale>-<speaker>-<quality> is also the path in the repository
          url = "https://huggingface.co/rhasspy/piper-voices/resolve/5ec43b186ef915a195d3ddbcfd9916e76c503c3f/${voice.language}/${
            lib.replaceStrings [ "-" ] [ "/" ] name
          }/${name}";
        in
        [
          {
            name = "${name}.onnx";
            path = pkgs.fetchurl {
              url = "${url}.onnx";
              hash = voice.model;
            };
          }
          {
            name = "${name}.onnx.json";
            path = pkgs.fetchurl {
              url = "${url}.onnx.json";
              hash = voice.config;
            };
          }
        ]
      ) voices
    )
  );

  defaultVoice = "en_GB-jenny_dioco-medium";

  # a resident server, since loading piper and a voice takes longer than speaking a sentence.
  # speech-dispatcher's rate -100..100 becomes piper's length scale 1.5..0.5
  speak = pkgs.writers.writeDash "piper-speak" ''
    ${lib.getExe pkgs.jq} -Rsc --arg voice "$1" --argjson rate "$2" '{text: ., $voice, length_scale: (1 - $rate / 200)}' \
      | ${lib.getExe pkgs.curl} -sf --json @- http://127.0.0.1:${toString port}/synthesize \
      | ${lib.getExe' config.services.pipewire.package "pw-play"} -
  '';

  ankiAddon = pkgs.anki-utils.buildAnkiAddon {
    pname = "niveum-tts";
    version = "1.0";
    src = pkgs.linkFarm "anki-niveum-tts" {
      "__init__.py" = ./anki.py;
      "tts.json" = (pkgs.formats.json { }).generate "tts.json" {
        piper = {
          url = "http://127.0.0.1:${toString port}/synthesize";
          voices = lib.attrNames voices;
        };
        espeak = {
          exe = lib.getExe pkgs.espeak-ng;
          locales = espeakLocales;
        };
        decks = ankiDecks;
      };
    };
  };
in
{
  systemd.user.services.piper = {
    description = "piper text to speech server";
    wantedBy = [ "default.target" ];
    serviceConfig.ExecStart = lib.escapeShellArgs [
      (lib.getExe pkgs.piper-tts-python)
      "-m"
      "piper.http_server"
      "--host"
      "127.0.0.1"
      "--port"
      (toString port)
      "--model"
      defaultVoice
      "--data-dir"
      voiceDir
    ];
  };

  # firefox's speechSynthesis (and so reader view's narrate) goes through speech-dispatcher
  services.speechd = {
    enable = true;
    config = ''
      AddModule "piper" "sd_generic" "piper.conf"
      DefaultModule piper
    '';
    modules.piper = ''
      GenericExecuteSynth "printf %s \'$DATA\' | ${speak} \'$VOICE\' $RATE"
      GenericDefaultCharset "utf-8"
      ${lib.concatStrings (
        lib.mapAttrsToList (name: voice: ''
          GenericLanguage "${voice.language}" "${voice.language}" "utf-8"
          AddVoice "${voice.language}" "${voice.type}" "${name}"
        '') voices
      )}
      DefaultVoice "${defaultVoice}"
    '';
  };

  # anki has no tts player for linux
  environment.systemPackages = [ (pkgs.anki.withAddons [ ankiAddon ]) ];
}
