#!/bin/sh
lidl() {
  echo LIDL
  curl -sSL 'https://endpoints.lidl-flyer.com/v3/region-overview/lidl/de-DE/0.json' \
    | jq -r '
      .categories
      | map(select(.name == "Filial-Angebote") | .subcategories | map(.flyers))
      | flatten
      | flatten
      | .[]
      | .pdfUrl
    '
}

aldi_nord() {
  echo ALDI nord
  echo 'https://magazine.aldi-nord.de/aldi-nord/aldi-aktuell/GetPDF.ashx'
  echo 'https://magazine.aldi-nord.de/aldi-nord/aldi-vorschau/GetPDF.ashx'
}

rewe_berlin() {(
  store_id=662366923
  publisher_id=1062

  echo REWE
  curl -sSL 'https://www.bonialserviceswidget.de/de/stores/'$store_id'/brochures?storeId='$store_id'&publisherId='$publisher_id | while read -r brochure_id; do
    curl -sSL 'https://www.bonialserviceswidget.de/de/v5/brochureDetails/'"$brochure_id"'?publisherId='$publisher_id  | jq -r .pdfUrl
  done
)}

kaufland() {(
  region_code=8920
  echo KAUFLAND
  curl -sSL https://filiale.kaufland.de/prospekte.html | htmlq --attribute href '.flyer a' | grep -Eo 'DE_de_KDZ[^/]*' | sed "s/_3000_/_${region_code}_/" | while read -r flyer_id; do
    curl -sSL "https://endpoints.leaflets.kaufland.com/v3/$flyer_id/flyer.json?regionCode=$region_code" | jq -r .flyer.pdfUrl
  done
)}

netto_schwarz() {
  echo 'NETTO (schwarz)'
  curl -sSL 'https://squid-api.tjek.com/v2/catalogs?dealer_ids=90f2VL&order_by=created' \
    | jq -r '.[] | .id' \
    | while read -r flyer_id; do
      curl -sSL "https://squid-api.tjek.com/v2/catalogs/$flyer_id/download" \
        | jq -r .pdf_url
    done
}

dir="$(mktemp -d)"
trap clean EXIT

clean() {
  rm -rf "$dir"
}

prospekt_url="$( (
  lidl
  aldi_nord
  rewe_berlin
  kaufland
  netto_schwarz
) | fzf)"

curl -sSL "$prospekt_url" -o "$dir/prospekt.pdf"
zathura "$dir/prospekt.pdf"
