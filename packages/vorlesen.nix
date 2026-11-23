# read german text aloud: vorlesen "Hallo Welt" | vorlesen < datei.txt
# or save it instead: vorlesen -o datei.mp3 < datei.txt
{
  writers,
  linkFarm,
  fetchurl,
  lib,
  piper-tts,
  alsa-utils,
  lame,
  gawk,
}:
let
  name = "de_DE-mls-medium";
  url = "https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/de/de_DE/mls/medium/${name}";
  # piper looks for the config next to the model, whatever -c says
  voice = linkFarm "piper-voice" {
    "${name}.onnx" = fetchurl {
      url = "${url}.onnx";
      hash = "sha256-ac0dKqWjWDmlGJZvzEkktfk+X4yUjtB1KxphatU/Zb8=";
    };
    "${name}.onnx.json" = fetchurl {
      url = "${url}.onnx.json";
      hash = "sha256-sK8cid39xy0y4BVymw6JuZ7sE8LIyqHbdIjZjp5XC0A=";
    };
  };
in
writers.writeBashBin "vorlesen" ''
  set -euo pipefail

  out=
  while getopts o: opt; do
    case $opt in
      o) out=$OPTARG ;;
      *) exit 2 ;;
    esac
  done
  shift $((OPTIND - 1))

  # piper synthesizes each line on its own, and this voice babbles on short
  # utterances like verse lines; one line per paragraph keeps them long
  if (($#)); then printf '%s\n' "$*"; else cat; fi |
    ${lib.getExe gawk} -v RS= '{ $1 = $1 } 1' |
    ${lib.getExe piper-tts} -m ${voice}/${name}.onnx -s "''${SPEAKER:-0}" --output-raw 2>/dev/null |
    if [[ $out ]]; then
      ${lib.getExe lame} --quiet -r -s 22.05 --bitwidth 16 --signed --little-endian -m m - "$out"
    else
      ${lib.getExe' alsa-utils "aplay"} -q -r 22050 -f S16_LE -t raw -c 1 -
    fi
''
