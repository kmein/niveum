#!/bin/sh
if [ "$#" -eq 1 ] && (echo "$1" | grep -Eq '[0-9]+\.[0-9]+'); then
  curl -Gs http://www.perseus.tufts.edu/hopper/CTS \
    -d request=GetPassage \
    -d "urn=urn:cts:greekLit:tlg0012.tlg002:$1" \
      | xmlstarlet sel -t -v 'cts:GetPassage//tei:div'
else
  echo you must supply BOOK.VERSE
fi
