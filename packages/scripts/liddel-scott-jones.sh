#!/bin/sh
set -efu

input="$*"

if echo "$input" | grep '^\w\+$'; then
  curl -sSL "https://lsj.gr/wiki/$input" | pup '#mw-content-text'
else
  curl -sSL "https://lsj.gr/wiki/$(echo "$input" | betacode)" \
    | pup ':parent-of(#English_(LSJ)) + p' \
    | sed 's/<span class="sense">/<p>&/g'
fi \
  | pandoc --standalone --variable=title:"$input" --from=html --to=man \
  | sed 's/\s\+\([:.,;]\)/\1/g;s/\s\+/ /g' \
  | man --local-file --pager="bat -p" -
