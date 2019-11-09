{ writeShellScriptBin, w3m, gnused }:
writeShellScriptBin "autorenkalender" ''
  ${w3m}/bin/w3m -dump https://gutenberg.spiegel.de/ \
    | ${gnused}/bin/sed -n '/Autorenkalender/,/━/p' \
    | head -n -2 \
    | tail -n +3
''
