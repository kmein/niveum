#!/bin/sh
set -efu

input="$*"

curl -sSL "https://www.sanskrit-lexicon.uni-koeln.de/scans/PWScan/2020/web/webtc/getword.php?key=$input&filter=roman&accent=yes&transLit=hk" \
  | pandoc --standalone --variable=title:"$input" --from=html --to=man \
  | sed 's/\s\+\([:.,;]\)/\1/g;s/\s\+/ /g' \
  | man --local-file --pager="bat -p" -
