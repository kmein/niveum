{ writeDashBin, wget, curl, jq }:
writeDashBin "instaget" ''
  for url in "$@"; do
    ${curl}/bin/curl -s "$url" \
      | grep display_url \
      | grep -o '{.*}' \
      | ${jq}/bin/jq -r '
          .entry_data.PostPage
          | .[].graphql.shortcode_media.edge_sidecar_to_children.edges
          | .[].node.display_url' \
      | xargs ${wget}/bin/wget
  done
''
