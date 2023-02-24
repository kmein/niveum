{
  writers,
  pup,
  curl,
  pandoc,
  man,
}:
writers.writeDashBin "InfSchMV" ''
  ${curl}/bin/curl -sSL https://www.berlin.de/corona/massnahmen/verordnung/ \
    | ${pup}/bin/pup .textile \
    | ${pandoc}/bin/pandoc -f html -t man -s \
    | ${man}/bin/man -l -
''
