#!/bin/sh

send_irc() {
  echo "
    USER $USER $(hostname) tolmoon $USER
    NICK musikkritiker
    JOIN #the_playlist
    PRIVMSG #the_playlist :$*
    QUIT
  " | nc irc.freenode.net 6667 >/dev/null
}

good="what a banger
ooh i love this song
this is top notch stuff!
nice!
noice!
yesss!
cool song!
i like this
that just sounds awesome!
that's a good song!
👍"

bad="how can anyone listen to this?
(╯°□°）╯ ┻━┻
skip this!
next, please! i'm suffering!
that's just bad music
nope
that sucks!
👎"

endpoint=prism.r:8001

case "$1" in
  good|like|cool|nice|noice|top|yass|yes|+)
    send_irc "$(echo "$good" | shuf -n1)" &
    curl -sS -XPOST "$endpoint/good"
  ;;
  skip|next|bad|sucks|no|nope|flop|-)
    send_irc "$(echo "$bad" | shuf -n1)" &
    curl -sS -XPOST "$endpoint/skip"
  ;;
  *)
    curl -sS -XGET "$endpoint/current" | jq
  ;;
esac
wait
