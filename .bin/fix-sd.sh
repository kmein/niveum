#!/bin/sh

drive="$1"
mountpoint="/media/sd-card-$(date +%s)"

trap clean EXIT
clean() {
  umount "$mountpoint"
  rm "$mountpoint"
}

filenames="$(fsck.exfat "$drive" 2>&1 | sed -nE "s/.* file '(.*?)' is not allocated.*/\1/p")"
mkdir "$mountpoint"
mount "$drive" "$mountpoint"

echo "$filenames" | while read -r filename; do
  find "$mountpoint" -type f -name "$filename" -exec rm {} \;
done

