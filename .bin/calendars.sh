#!/bin/sh

directory="$(mktemp -d)"
trap clean EXIT
clean() {
  rm -rf "$directory"
}

year=2022
output=/tmp/$year.pdf

for month in $(seq 1 12); do
  printf "\r%d" "$month" 1>&2
  astrolog -zN Berlin -qm "$month" "$year" -X -K -XA -R Uranus Neptune Pluto -Xr -Xm -Xb -Xo "$(printf "%s/%02d.bmp" "$directory" "$month")" -Xw 1080 720 2>/dev/null
done
printf "\r"

convert "$directory/*.bmp" "$output"
echo "$output"
