#!/bin/sh
source_image="$1"

if [ -e "$source_image" ]; then
  convert -type Grayscale -resize 758x1024 "$source_image" "suspend.jpg"
else
  echo >/dev/stderr "$1 must exist."
  exit 1
fi
