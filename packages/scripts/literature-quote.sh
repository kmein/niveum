#!/bin/sh
ROW=$(curl -Ls http://kmein.github.io/logotheca/quotes.csv | shuf -n1)

(
  printf '%s\n\n— %s: _%s_, %s\n' \
    "$(echo "$ROW" | xsv select 4)" \
    "$(echo "$ROW" | xsv select 1)" \
    "$(echo "$ROW" | xsv select 2)" \
    "$(echo "$ROW" | xsv select 3 | tr : ,)"
) | sed 's/ | /\n/g;s/ || /\n\n/g;s/"\(.*\)"/\1/'
