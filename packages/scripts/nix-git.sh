#!/bin/sh
nix-prefetch-git "$@" 2> /dev/null \
  | jq -r '"rev = \"\(.rev)\";\nsha256 = \"\(.sha256)\";"'
