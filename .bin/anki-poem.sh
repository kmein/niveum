#!/bin/sh
file="${1?please supply a poetry file}"
[ -f "$file" ] || {
  echo "'$file' is no file"
  exit 1
}

poem="$(mktemp)"
clean () {
  rm "$poem"
}
trap clean EXIT
sed '/^$/d' "$file" > "$poem"

htmlize() {
  awk 'ORS="<br/>"' \
    | head -c -5          # remove final <br/> characters
}

for line_number in $(seq 1 "$(wc -l "$poem" | cut -d' ' -f1)"); do
  if [ "$line_number" -gt 3 ] && [ "$line_number" -gt 1 ]; then
    sed -n "$((line_number - 3)),$((line_number - 1))p" "$poem"
  else
    sed -n "1,$((line_number - 1))p" "$poem"
  fi | htmlize
  printf '\t'
  sed -n "${line_number},+1p" "$poem" | htmlize
  printf '\n'
done
