{ writers, chromium }:
writers.writeDashBin "cro" ''
  ${chromium}/bin/chromium \
    --disable-sync \
    --no-default-browser-check \
    --no-first-run \
    --user-data-dir="$(mktemp -d)" \
    --incognito \
    "$@"
''
