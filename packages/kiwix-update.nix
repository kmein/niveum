# Pin the newest dated file of every ZIM in a kiwix pin file (configs/kiwix/zims.json)
{
  writers,
  curl,
  jq,
  coreutils,
  gnugrep,
  gnused,
  lib,
}:
writers.writeDashBin "kiwix-update" ''
  set -efu
  export PATH=${
    lib.makeBinPath [
      curl
      jq
      coreutils
      gnugrep
      gnused
    ]
  }:$PATH

  file=''${1:-configs/kiwix/zims.json}
  tmp=$(mktemp)
  trap 'rm -f "$tmp"' EXIT

  pins=$(cat "$file")
  for name in $(printf '%s' "$pins" | jq -r 'keys[]'); do
    # download.kiwix.org/zim/<name>.zim redirects to lb.download.kiwix.org/zim/<category>/<name>_<date>.zim
    url=$(
      curl -fsSIL "https://download.kiwix.org/zim/$name.zim" \
        | tr -d '\r' \
        | grep -Eio "^location: https://[a-z.]*download\.kiwix\.org/zim/[^/]+/''${name}_[0-9-]+\.zim$" \
        | sed 's/^[^ ]* //; s|//lb\.download|//download|' \
        | head -n 1
    )
    if [ -z "$url" ]; then
      echo "kiwix-update: cannot resolve $name" >&2
      exit 1
    fi
    hex=$(curl -fsSL "$url.sha256" | cut -d ' ' -f 1)
    hash="sha256-$(printf '%s' "$hex" | basenc --base16 -d | base64 -w 0)"
    echo "$name: $url" >&2
    pins=$(printf '%s' "$pins" | jq --arg name "$name" --arg url "$url" --arg hash "$hash" '.[$name] = { url: $url, hash: $hash }')
  done
  printf '%s\n' "$pins" | jq -S . > "$tmp"
  cat "$tmp" > "$file"
''
