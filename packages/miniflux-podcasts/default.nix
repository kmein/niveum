{
  lib,
  writers,
  haskellPackages,
}:
let
  miniflux-podcasts = writers.writeHaskellBin "miniflux-podcasts" {
    libraries = with haskellPackages; [
      aeson
      async
      http-client
      http-client-tls
      http-types
    ];
    ghcArgs = [ "-threaded" ];
  } ./miniflux-podcasts.hs;
in
# the program works in the current directory: files, ledger and .env live there
writers.writeDashBin "miniflux-podcasts" ''
  set -e
  : "''${MINIFLUX_URL:=https://feed.${lib.niveum.domain}/v1}"
  export MINIFLUX_URL
  dir="$HOME/mobile/audio/Text/podcasts"
  mkdir -p "$dir"
  cd "$dir"
  exec ${miniflux-podcasts}/bin/miniflux-podcasts "$@"
''
