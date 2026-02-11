{ nix, writeShellScriptBin }:
writeShellScriptBin "yt-dlp-master" ''
  args=$@
  ${nix}/bin/nix-shell -p '(yt-dlp.overrideAttrs (_: {
    src = builtins.fetchTree "github:yt-dlp/yt-dlp";
    patches = [];
    postPatch = "python devscripts/update-version.py 0.99";
  }))' -p deno --run "yt-dlp $args"
''
