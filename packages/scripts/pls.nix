{ lib, pkgs }:
let
  playlistAPI = "https://radio.lassul.us";

  sendIRC = pkgs.writers.writeDash "send-irc" ''
    ${pkgs.ircaids}/bin/ircsink \
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
in
pkgs.writers.writeDashBin "pls" ''
  case "$1" in
    good|like|cool|nice|noice|top|yup|yass|yes|+)
      ${pkgs.curl}/bin/curl -sS -XPOST "${playlistAPI}/good"
      echo ${lib.escapeShellArg (lib.concatStringsSep "\n" messages.good)} | shuf -n1 | ${sendIRC}
    ;;
    skip|next|bad|sucks|no|nope|flop|-)
      ${pkgs.curl}/bin/curl -sS -XPOST "${playlistAPI}/skip"
      echo ${lib.escapeShellArg (lib.concatStringsSep "\n" messages.bad)} | shuf -n1 | ${sendIRC}
    ;;
    recent)
      ${pkgs.curl}/bin/curl -sS -XGET "${playlistAPI}/recent" | tac | head
    ;;
    *)
      ${pkgs.curl}/bin/curl -sS -XGET "${playlistAPI}/current" \
        | ${pkgs.miller}/bin/mlr --ijson --oxtab cat \
        | ${pkgs.gnused}/bin/sed -n '/artist\|title\|youtube/p'
    ;;
  esac
  wait
''
