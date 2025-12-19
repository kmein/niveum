{ coreutils, chromium }:
chromium.override {
  commandLineArgs = [
    "--disable-sync"
    "--no-default-browser-check"
    "--no-first-run"
    "--user-data-dir=$(${coreutils}/bin/mktemp -d)"
    "--incognito"
  ];
}
