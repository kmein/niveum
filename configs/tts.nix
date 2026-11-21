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
    de_DE-mls-medium = {
      language = "de";
      type = "MALE1";
      model = "sha256-ac0dKqWjWDmlGJZvzEkktfk+X4yUjtB1KxphatU/Zb8=";
      config = "sha256-sK8cid39xy0y4BVymw6JuZ7sE8LIyqHbdIjZjp5XC0A=";
    };
    en_GB-jenny_dioco-medium = {
      language = "en";
      type = "FEMALE1";
      model = "sha256-RpxjDSCeE53TkqZr9KveSrhjkKAmnB5HtOXXzoFSawE=";
      config = "sha256-qaepOjF8mjy2Vj436wV9+e8JwGGIqKQ0Gw/LWMulTdQ=";
    };
  };

  voiceDir = pkgs.linkFarm "piper-voices" (
    lib.concatLists (
      lib.mapAttrsToList (
        name: voice:
        let
          # <locale>-<speaker>-<quality> is also the path in the repository
          url = "https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/${voice.language}/${
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

  python = pkgs.python3.withPackages (ps: [ (ps.toPythonModule pkgs.piper-tts) ]);

  # a resident server, since loading piper and a voice takes longer than speaking a sentence.
  # speech-dispatcher's rate -100..100 becomes piper's length scale 1.5..0.5
  speak = pkgs.writers.writeDash "piper-speak" ''
    ${lib.getExe pkgs.jq} -Rsc --arg voice "$1" --argjson rate "$2" '{text: ., $voice, length_scale: (1 - $rate / 200)}' \
      | ${lib.getExe pkgs.curl} -sf --json @- http://127.0.0.1:${toString port}/ \
      | ${lib.getExe' config.services.pipewire.package "pw-play"} -
  '';
in
{
  systemd.user.services.piper = {
    description = "piper text to speech server";
    wantedBy = [ "default.target" ];
    serviceConfig.ExecStart = lib.escapeShellArgs [
      (lib.getExe python)
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
}
