#!/bin/sh
curl -sSL 'https://kalbu.vdu.lt/wp-admin/admin-ajax.php' -F action=text_accents -F body="$(cat)" \
  | jq -r .message \
  | if [ "$1" = "--json" ]
    then jq .textParts
    else jq -r '
      .textParts
      | map(if has("accented") then .accented else .string end)
      | join("")
    '
    fi
