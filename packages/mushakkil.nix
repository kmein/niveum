# Add Arabic diacritics (tashkeel) to text via alsharekh.org
{
  writers,
  curl,
  jq,
}:
writers.writeDashBin "mushakkil" ''
  ${curl}/bin/curl -sSL 'https://diac.alsharekh.org/Diac/DiacText' \
    -H "Content-Type: application/json" \
    --data-raw "$(${jq}/bin/jq --raw-input '{word: ., type: 1}')" \
    --compressed \
    | ${jq}/bin/jq -r .diacWord
''
