{
  writers,
  lib,
  xan,
  curl,
  gnused,
}:
writers.writeDashBin "literature-quote" ''
  PATH=$PATH:${lib.makeBinPath [xan curl gnused]}
  ROW=$(curl -Ls http://kmein.github.io/logotheca/quotes.csv | shuf -n1)
  (
    QUOTE="$(echo "$ROW" | xan select 3)"

    echo "$QUOTE" | sed 's/^"//;s/"$//;s/\s*||\s*/\n\n/g;s/\s*|\s*/\n/g'
    echo

    AUTHOR="$(echo "$ROW" | xan select 0)"
    # Prepare the output
    ATTRIBUTION="($AUTHOR"

    SOURCE="$(echo "$ROW" | xan select 1)"

    # Add SOURCE if it's not empty
    if [ -n "$SOURCE" ]; then
      ATTRIBUTION="$ATTRIBUTION: $SOURCE"
    fi

    LOC="$(echo "$ROW" | xan select 2 | sed 's/""//;s/-/–/g')"
    # Add LOC if it's not empty
    if [ -n "$LOC" ]; then
      ATTRIBUTION="$ATTRIBUTION, $LOC"
    fi

    ATTRIBUTION="$ATTRIBUTION)"

    echo "$ATTRIBUTION"
  )
''
