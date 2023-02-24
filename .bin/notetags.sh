#!/bin/sh

# inspired by https://github.com/connermcd/bin/blob/1d38cb98812906d8b95dc6e51e1149e29261617d/notetags

cd "$HOME/notes/" || exit

[ -f tags ] && rm tags
grep -r 'tags:' ./* | while read -r line; do
  file=$(echo "$line" | cut -d: -f1)
  unparsed_tags=$(echo "$line" | cut -d: -f3) #
  tags=$(echo "$unparsed_tags" | sed -e 's/tags: *//g' -e 's/[][,]//g')
  for tag in $tags; do
    echo "$tag	$file	/^$unparsed_tags$/;" >> tags
  done
done
