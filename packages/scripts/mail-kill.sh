#! /bin/sh
set -efu

if ! notmuch search --exclude=false tag:deleted | tac | grep .; then
  echo 'No killed mail.'
  exit 1
fi

printf 'want do rm these mail? [y/N] '
read REPLY
case "$REPLY" in
  y|Y) :;; # continue
  *)
    echo 'abort.'
    exit 2
    ;;
esac

notmuch search --output=files --exclude=false tag:deleted | xargs -l rm -v
notmuch new
