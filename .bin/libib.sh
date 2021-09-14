#!/usr/bin/env bash
session_id=7b638c194d9bda74f80043045018cc9e

declare -A libraries

libraries["Literatur"]=344428
libraries["Sprache"]=344160
libraries["Miscellanea"]=344427
libraries["Wissenschaft"]=344429
libraries["Relicta"]=565920

for library in ${!libraries[@]}
do
  curl -sSL 'https://www.libib.com/library/functions/csv-export.php' -H "Cookie: PHPSESSID=$session_id" -d export="${libraries[$library]}" > "$library.csv"
done
