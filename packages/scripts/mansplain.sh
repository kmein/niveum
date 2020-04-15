#!/bin/sh
# https://www.youtube.com/watch?v=8E8sUNHdzG8
man -k . \
  | cut -d" " -f1,2 \
  | dmenu -l 5 \
  | sed 's/\(.*\) (\(.*\))/\2 \1/' \
  | xargs -r man -t \
  | zathura -
