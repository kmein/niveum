#!/bin/sh
set -efu

if echo "$1" | grep -Eq '[[:digit:]]{5}'; then
  PLZ="$1"
else
  echo >&2 "Usage: $0 PLZ"
  exit 1
fi

lieferando_dir=/tmp/lieferando
mkdir -p "$lieferando_dir/$PLZ"

fetch_restaurants() {
  cache_path="$lieferando_dir/$PLZ.json"

  if [ -r "$cache_path" ]; then
    cat "$cache_path"
  else
    w3m -dump_source "http://www.lieferando.de/$PLZ" \
      | gunzip \
      | sed -n '/var restaurants/,/];$/p' \
      | sed 's/var restaurants =//;$s/;$//' \
      | prettier --parser=json \
      | jq '
        map({
          name: .[30] | .name,
          category: .[30] |.categories | split(", "),
          url: "http://lieferando.de\(.[30] | .url)",
          minutes: .[19],
          minimum: .[10],
          delivery: .[14]
        })' \
      | tee "$cache_path"
  fi
}

fetch_menu() {
  [ $# -eq 1 ] || exit 1

  slug="$(echo "$1" | sed 's!.*/!!')"
  cache_path="$lieferando_dir/$PLZ/$slug.json"

  if [ -r "$cache_path" ]; then
    cat "$cache_path"
  else
    w3m -dump_source "$1" \
      | gunzip \
      | sed -n '/var MenucardProducts/,/\];/p' \
      | sed 's/var MenucardProducts =//;s/;$//' \
      | jq -r '
        unique_by(.productId)
        | group_by(.categoryId)
        | flatten
      ' \
      | tee "$cache_path"
  fi
}

data="$(fetch_restaurants)"

# echo "$data" | jq -c '.[]' | while read -r restaurant; do
#   fetch_menu "$(echo "$restaurant" | jq -r .url)"
# done

selected_categories="$(echo "$data" | jq -r 'map(.category) | flatten | unique | .[]' | fzf -m)"

selected_restaurant_url="$(echo "$selected_categories" | jq --argjson restaurants "$data" -sRr '
  split("\n")[:-1] as $categories
  | $restaurants[]
  | select(.category - $categories != .category)
  | "\(.name) [🚴\(.minutes)min 💰\(.minimum)€ + \(.delivery)€] (\(.url))"
  ' \
  | fzf \
  | sed 's/.*(//;s/)$//'
)"

fetch_menu "$selected_restaurant_url" \
  | jq -r '.[] | "\(.price)\t\(.name)"' \
  | fzf -m \
  | awk '{print $0; sum += $1} END {print "-----"; print sum}'
