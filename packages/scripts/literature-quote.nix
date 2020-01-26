{ writeShellScriptBin, curl, xsv, gnused }:
writeShellScriptBin "literature-quote" ''
  ROW=$(${curl}/bin/curl -Ls http://kmein.github.io/quotes/quotes.csv | shuf -n1)

  (
    printf '%s\n\n— %s: _%s_, %s\n' \
      "$(echo "$ROW" | ${xsv}/bin/xsv select 4)" \
      "$(echo "$ROW" | ${xsv}/bin/xsv select 1)" \
      "$(echo "$ROW" | ${xsv}/bin/xsv select 2)" \
      "$(echo "$ROW" | ${xsv}/bin/xsv select 3 | tr : ,)"
  ) | ${gnused}/bin/sed 's/ | /\n/g;s/ || /\n\n/g;s/"\(.*\)"/\1/'
''
