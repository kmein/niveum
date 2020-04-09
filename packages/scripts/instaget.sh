#!/bin/sh
for url in "$@"; do
  json="$(curl -s "$url" \
    | grep display_url \
    | grep -o '{.*}')"

  echo "$json" \
    | jq -r '
        .entry_data.PostPage
        | .[].graphql.shortcode_media.edge_sidecar_to_children.edges
        | .[].node.display_url' \
    | xargs wget

  echo "$json" \
    | jq -r  '
      .entry_data.PostPage
      | .[].graphql.shortcode_media.display_url' \
    | xargs wget
done
