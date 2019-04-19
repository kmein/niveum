{ writeDashBin, nix-prefetch-git, jq }:
writeDashBin "nix-git" ''
  ${nix-prefetch-git}/bin/nix-prefetch-git "$@" 2> /dev/null | ${jq}/bin/jq -r '"rev = \"\(.rev)\";\nsha256 = \"\(.sha256)\";"'
''
