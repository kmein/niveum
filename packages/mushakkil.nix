# Add Arabic diacritics (tashkeel) to text via alsharekh.org
{
  lib,
  writers,
  curl,
  jq,
}:
writers.writeDashBin "mushakkil" ''
  ${lib.getExe curl} -sSL 'https://diac.alsharekh.org/Diac/DiacText' \
    -H "Content-Type: application/json" \
    --data-raw "$(${lib.getExe jq} --raw-input '{word: ., type: 1}')" \
    --compressed \
    | ${lib.getExe jq} -r .diacWord
''
