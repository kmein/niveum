#!/bin/sh
fd -t f -H -I \
  | fzf -m --preview="bat {}" \
  | xargs -ro -d "\n" "$EDITOR" 2>&-
