{
  lib,
  writers,
  miller,
  gnused,
  curl,
  nur,
}: let
  playlistAPI = "https://radio.lassul.us";

  sendIRC = writers.writeDash "send-irc" ''
    ${nur.repos.mic92.ircsink}/bin/ircsink \
      --nick musikkritiker \
      --server irc.hackint.org \
      --port 6697 \
      --secure \
      --target '#the_playlist' >/dev/null 2>&1
  '';

  messages.good = [
    "what a banger"
    "ooh i love this song"
    "this is top notch stuff!"
    "nice!"
    "noice!"
    "yesss!"
    "cool song!"
    "i like this"
    "that just sounds awesome!"
    "that's a good song!"
    "👍"
    "vibin'"
  ];
  messages.bad = [
    "how can anyone listen to this?"
    "(╯°□°）╯ ┻━┻"
    "skip this!"
    "next, please! i'm suffering!"
    "that's just bad music"
    "nope"
    "that sucks!"
    "👎"
    "turn that down"
    "make it stooop"
    "noooo"
  ];

  messages.neutral = [
    "meh"
    "i have no opinion about this song"
    "idk man"
  ];
in
  writers.writeDashBin "pls" ''
    case "$1" in
      good|like|cool|nice|noice|top|yup|yass|yes|+)
        ${curl}/bin/curl -sS -XPOST "${playlistAPI}/good"
        echo ${lib.escapeShellArg (lib.concatStringsSep "\n" messages.good)} | shuf -n1 | ${sendIRC}
      ;;
      skip|next|bad|sucks|no|nope|flop|-)
        ${curl}/bin/curl -sS -XPOST "${playlistAPI}/skip"
        echo ${lib.escapeShellArg (lib.concatStringsSep "\n" messages.bad)} | shuf -n1 | ${sendIRC}
      ;;
      0|meh|neutral)
        echo ${lib.escapeShellArg (lib.concatStringsSep "\n" messages.neutral)} | shuf -n1 | ${sendIRC}
      ;;
      say|msg)
        shift
        echo "$@" | ${sendIRC}
      ;;
      recent)
        ${curl}/bin/curl -sS -XGET "${playlistAPI}/recent" | tac | head
      ;;
      *)
        ${curl}/bin/curl -sS -XGET "${playlistAPI}/current" \
          | ${miller}/bin/mlr --ijson --oxtab cat \
          | ${gnused}/bin/sed -n '/artist\|title\|youtube/p'
      ;;
    esac
    wait
  ''
