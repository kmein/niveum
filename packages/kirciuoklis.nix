{
  writers,
  curl,
  jq,
}:
writers.writeDashBin "kirciuoklis" ''
  ${curl}/bin/curl -sSL 'https://kalbu.vdu.lt/wp-admin/admin-ajax.php' -F action=text_accents -F body="$(cat)" \
  | ${jq}/bin/jq -r .message \
  | if [ "$1" = "--json" ]
    then ${jq}/bin/jq .textParts
    else ${jq}/bin/jq -r '
      .textParts
      | map(if has("accented") then .accented else .string end)
      | join("")
    '
    fi
''
