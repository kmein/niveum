{ writeShellScriptBin, wget, curl, jq }:
writeShellScriptBin "instaget" ''
  for url in "$@"; do
    json="$(${curl}/bin/curl -s "$url" \
      | grep display_url \
      | grep -o '{.*}')"

    echo "$json"
      | ${jq}/bin/jq -r '
          .entry_data.PostPage
          | .[].graphql.shortcode_media.edge_sidecar_to_children.edges
          | .[].node.display_url' \
      | xargs ${wget}/bin/wget

    echo "$json"
      | ${jq}/bin/jq -r  '
        .entry_data.PostPage
        | .[].graphql.shortcode_media.display_url' \
      | xargs ${wget}/bin/wget
  done
''
