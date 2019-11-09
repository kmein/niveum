{ writeShellScriptBin, curl }:
writeShellScriptBin "wttr" ''
  ${curl}/bin/curl -s -H "Accept-Language: ''${LANG%_*}" --compressed "wttr.in/''${1-}?0"
''
