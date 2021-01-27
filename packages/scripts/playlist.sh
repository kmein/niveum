#!/bin/sh

send_irc() {
  echo "
    USER $USER $(hostname) tolmoon $USER
    NICK musikkritiker
    JOIN #the_playlist
    PRIVMSG #the_playlist $*
    QUIT
  " | nc irc.freenode.net 6667 >/dev/null
}



endpoint=prism.r:8001

case "$1" in
  good|like|cool|nice|yes|+)
    send_irc 'nice!' &
    curl -sS -XPOST "$endpoint/good"
  ;;
  skip|next|bad|sucks|no|nope|-)
    send_irc 'sucks' &
    curl -sS -XPOST "$endpoint/skip"
  ;;
  *)
    curl -sS -XGET "$endpoint/current" | jq
  ;;
esac
wait
