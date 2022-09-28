#! /usr/bin/env nix-shell
#! nix-shell -i sh -p coreutils byzanz xorg.xwininfo gnused

# shellcheck shell=sh
# ref https://gist.github.com/aforemny/0994cb7f06ea30d56c8b9681ff5d2054

set -eux

eval "$(xwininfo | \
  sed -n -e 's/^ \+Absolute upper-left X: \+\([0-9]\+\).*/x=\1/p' \
         -e 's/^ \+Absolute upper-left Y: \+\([0-9]\+\).*/y=\1/p' \
         -e 's/^ \+Width: \+\([0-9]\+\).*/w=\1/p' \
         -e 's/^ \+Height: \+\([0-9]\+\).*/h=\1/p')"

trap "pkill -f 'sleep 360d'" INT
byzanz-record -e "sleep 360d" -c -x $x -y $y -w $w -h $h "$@"
