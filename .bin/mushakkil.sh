#!/bin/sh
curl -sSL 'https://diac.alsharekh.org/Diac/DiacText' \
  -H "Content-Type: application/json" \
  --data-raw "$(jq --raw-input '{word: ., type: 1}')" \
  --compressed \
  | jq -r .diacWord
