{ pkgs }:
let
  inherit (pkgs) lib;

  irc = {
    host = "irc.hackint.org";
    port = 6697;
    tls = true;
    channel = "#the_playlist";
    nick = "musikkritiker";
  };

  playlistAPI = "prism.r:8001";

  sendIRC = pkgs.writers.writeDash "send-irc" ''
    ${pkgs.nur.repos.mic92.untilport}/bin/untilport ${irc.host} ${toString irc.port} && \
    ${pkgs.nur.repos.mic92.irc-announce}/bin/irc-announce \
      ${irc.host} ${toString irc.port} ${irc.nick} ${lib.escapeShellArg irc.channel} ${toString (if irc.tls then 1 else 0)} \
      "$*"
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
  ];
in
pkgs.writers.writeDashBin "pls" ''
  case "$1" in
    good|like|cool|nice|noice|top|yass|yes|+)
      # ${sendIRC} "$(echo "${lib.concatStringsSep "\n" messages.good}" | shuf -n1)" &
      ${pkgs.curl}/bin/curl -sS -XPOST "${playlistAPI}/good"
    ;;
    skip|next|bad|sucks|no|nope|flop|-)
      # ${sendIRC} "$(echo "${lib.concatStringsSep "\n" messages.bad}" | shuf -n1)" &
      ${pkgs.curl}/bin/curl -sS -XPOST "${playlistAPI}/skip"
    ;;
    *)
      ${pkgs.curl}/bin/curl -sS -XGET "${playlistAPI}/current" | ${pkgs.jq}/bin/jq
    ;;
  esac
  wait
''
