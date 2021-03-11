#!/bin/sh
set -efu

cache_file=/tmp/rfc-index.txt

fetch_index() {
  if [ -f "$cache_file" ]
  then cat "$cache_file"
  else curl -sSL https://tools.ietf.org/rfc/index | tee "$cache_file"
  fi
}

rfc_list="$(fetch_index \
  | pup 'pre text{}' \
  | awk '$0 != "" {printf "%s",$0} $0 == "" {printf "\n"}' \
  | sed 's/\s\+/ /g' \
  | sed 'n;d' \
  | grep '^RFC[[:digit:]]' \
  | sed 's/RFC\([[:digit:]]\+\)/\1\t/;s/\. .*//'
)"

selection="$(echo "$rfc_list" | fzf | awk '{print $1}')"

curl -sSL "https://tools.ietf.org/rfc/rfc$selection.txt" | less
