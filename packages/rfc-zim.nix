# Build a ZIM of all RFCs from rfc-editor.org, upload it to the cyberlocker and
# pin it in a kiwix pin file (configs/kiwix/rfc.json). Kiwix publishes no RFCs.
{
  writers,
  rsync,
  zim-tools,
  jq,
  imagemagick,
  curl,
  coreutils,
  gnugrep,
  gnused,
  lib,
}:
writers.writeDashBin "rfc-zim" ''
  set -efu
  export PATH=${
    lib.makeBinPath [
      rsync
      zim-tools
      jq
      imagemagick
      curl
      coreutils
      gnugrep
      gnused
    ]
  }:$PATH

  pin=''${1:-configs/kiwix/rfc.json}
  # dated, so that a later run never changes what an existing pin points to
  file=rfc_en_all_$(date +%Y-%m-%d).zim
  zim=$PWD/$file
  work=$(mktemp -d)
  trap 'rm -rf "$work"' EXIT

  # bulk tarballs are gone, rsync is the only bulk download
  rsync -rt --include='rfc[0-9]*.html' --exclude='*' rsync.rfc-editor.org::rfcs/ "$work/html/"
  rsync -rt --include='rfc[0-9]*.json' --exclude='*' rsync.rfc-editor.org::rfcs/ "$work/json/"

  # cross-references point at rfc-editor.org (newer RFCs) or ./rfcN (older ones)
  find "$work/html" -name 'rfc*.html' -exec sed -E -i \
    's,href="(\./|https://www\.rfc-editor\.org/rfc/)rfc([0-9]+)(\.html)?(["#]),href="rfc\2.html\4,g' {} +

  find "$work/json" -name 'rfc*.json' -exec cat {} + > "$work/rfcs.json"
  jq -rs 'sort_by(.doc_id[3:] | tonumber)[] | [(.doc_id | ascii_downcase), .doc_id[3:], (.title | @html), (.status | @html)] | @tsv' \
    "$work/rfcs.json" > "$work/rfcs.tsv"

  tab=$(printf '\t')
  {
    printf '<!DOCTYPE html>\n<html><head><meta charset="utf-8"><title>RFCs</title></head><body>\n<h1>RFCs</h1>\n<table>\n'
    while IFS=$tab read -r id number title status; do
      page=$work/html/$id.html
      [ -e "$page" ] || continue
      # older RFCs are a bare <pre>, without a title for kiwix to show and search
      if ! grep -qi '<html' "$page"; then
        {
          printf '<!DOCTYPE html>\n<html><head><meta charset="utf-8"><title>RFC %s: %s</title></head><body>\n' "$number" "$title"
          cat "$page"
          printf '</body></html>\n'
        } > "$page.new"
        mv "$page.new" "$page"
      fi
      printf '<tr><td><a href="%s.html">RFC %s</a></td><td>%s</td><td>%s</td></tr>\n' "$id" "$number" "$title" "$status"
    done < "$work/rfcs.tsv"
    printf '</table>\n</body></html>\n'
  } > "$work/html/index.html"

  magick -size 48x48 xc:'#a31f34' "$work/html/illustration.png"

  zimwriterfs \
    --welcome=index.html \
    --illustration=illustration.png \
    --language=eng \
    --name=rfc_en_all \
    --title=RFCs \
    --description='Requests for Comments from the RFC Editor' \
    --creator='RFC Editor' \
    --publisher=kmein \
    --source=https://www.rfc-editor.org \
    "$work/html" "$zim"

  # c.r takes uploads only over retiolum and serves them as c.krebsco.de
  if ! curl -fSs --data-binary @"$zim" "http://c.r/$file"; then
    echo "rfc-zim: upload failed; $zim is kept" >&2
    exit 1
  fi
  hex=$(sha256sum "$zim" | cut -d ' ' -f 1)
  hash="sha256-$(printf '%s' "$hex" | basenc --base16 -d | base64 -w 0)"
  jq -n --arg url "https://c.krebsco.de/$file" --arg hash "$hash" '{ url: $url, hash: $hash }' > "$pin"
  rm "$zim"
  echo "rfc-zim: https://c.krebsco.de/$file" >&2
''
