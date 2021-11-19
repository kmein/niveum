#!/bin/sh

filepath="$(shuf --head-count=1)"
pages="$(pdfinfo "$filepath" | awk '/^Pages:/{print $2}')"
random_page="$(shuf --input-range="1-$pages" --head-count=1)"
zathura --page="$random_page" "$filepath"
