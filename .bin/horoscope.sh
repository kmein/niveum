#!/bin/sh
set -efu

# Berlin:  -d lodeg=13 -d lomin=22 -d losec=41 -d lodir=E -d ladeg=52 -d lamin=27 -d lasec=42 -d ladir=N -d usecoords=1 \
# Kassel:  -d lodeg=9 -d lomin=32 -d losec=5 -d lodir=E -d ladeg=51 -d lamin=18 -d lasec=17 -d ladir=N -d usecoords=1 \


[ $# -eq 1 ] || {
  echo >&2 Usage: "$0" TIMESTAMP
  exit 1
}

export TZ=UTC

chart_path="$(mktemp /tmp/chart_XXX.pdf)"

timestamp="$1"

year="$(date -d "@$timestamp" +%Y)"
month="$(date -d "@$timestamp" +%m)"
day="$(date -d "@$timestamp" +%d)"
hour="$(date -d "@$timestamp" +%H)"
minute="$(date -d "@$timestamp" +%M)"

curl -sSL 'https://edifyingfellowship.org/astro/' \
  -d lodeg=9 -d lomin=32 -d losec=5 -d lodir=E -d ladeg=51 -d lamin=18 -d lasec=17 -d ladir=N -d usecoords=1 \
  -d ybyr="$year" -d ybmo="$month" -d ybdy="$day" -d ybhr="$hour" -d ybmi="$minute" -d ybsc=0 -d ybtz="$TZ" \
  -d currenttime=0 \
  -d title="$timestamp" \
  -d options[]=VancouverWheel -d options[]=Arrow -d options[]=XBold -d options[]=HouseLabels -d options[]=Placidus \
  -d options[]=Sun -d options[]=Moon -d options[]=Mercury -d options[]=Venus -d options[]=Mars -d options[]=Jupiter -d options[]=Saturn -d options[]=Uranus -d options[]=Neptune -d options[]=Pluto -d options[]=Ascendant -d options[]=MC -d options[]=Lilith -d options[]=MeanNode -d options[]=TrueNode \
  -d aspectpct=100 -d format=PDF -d Submit= -o "$chart_path"

zathura "$chart_path"
