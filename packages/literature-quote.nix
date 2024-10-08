{
  writers,
  lib,
  xsv,
  curl,
  gnused,
}:
writers.writeDashBin "literature-quote" ''
  PATH=$PATH:${lib.makeBinPath [xsv curl gnused]}
  ROW=$(curl -Ls http://kmein.github.io/logotheca/quotes.csv | shuf -n1)
  (
    QUOTE="$(echo "$ROW" | xsv select 4)"

    echo "$QUOTE" | sed 's/^"//;s/"$//;s/\s*||\s*/\n\n/g;s/\s*|\s*/\n/g'
    echo

    AUTHOR="$(echo "$ROW" | xsv select 1)"
    # Prepare the output
    ATTRIBUTION="($AUTHOR"

    SOURCE="$(echo "$ROW" | xsv select 2)"

    # Add SOURCE if it's not empty
    if [ -n "$SOURCE" ]; then
      ATTRIBUTION="$ATTRIBUTION: $SOURCE"
    fi

    LOC="$(echo "$ROW" | xsv select 3 | sed 's/""//;s/-/–/g')"
    # Add LOC if it's not empty
    if [ -n "$LOC" ]; then
      ATTRIBUTION="$ATTRIBUTION, $LOC"
    fi

    ATTRIBUTION="$ATTRIBUTION)"

    echo "$ATTRIBUTION"
  )
''
