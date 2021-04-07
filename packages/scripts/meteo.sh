#! /bin/sh
# usage: meteo --list
# usage: meteo --update
# usage: meteo STATION
set -efu

# TODO XDG
CONFIG_DIR=$HOME/.config/wetter
STATIONS_FILE=$CONFIG_DIR/stations.json

case ${1-} in
  --list)
    sed -n 's/^\s*\(--[^)]\+\))$/\1/p' "$0"
    jq -r -n \
    --slurpfile stations_file "$STATIONS_FILE" \
    '
      $stations_file[0] as $known_stations |

      $known_stations | keys[]
    '
    exit
  ;;
  --update)
    mkdir -p "$(dirname "$STATIONS_FILE")"
    exec >"$STATIONS_FILE"

    curl -fsS http://wetterstationen.meteomedia.de/ |
    jq -Rrs '
      def decodeHTML:
        gsub("&auml;";"ä") |
        gsub("&ouml;";"ö") |
        gsub("&uuml;";"ü") |
        gsub("&Auml;";"Ä") |
        gsub("&Ouml;";"Ö") |
        gsub("&Uuml;";"Ü") |
        gsub("&szlig;";"ß")
        ;
      [
        match(".*<option value=\"/\\?map=Deutschland&station=(?<station>[0-9]+)\">(?<name>[^<]+)</option>";"g")
        .captures |
        map({"\(.name)":(.string)}) |
        add |
        {"\(.name|decodeHTML)":(.station|tonumber)}
      ] |
      add
    '
    exit
  ;;
esac

# set -x

station=${1-103840}
station=$(jq -e -n \
--arg station "$station" \
--slurpfile stations_file "$STATIONS_FILE" \
'
  $stations_file[0] as $known_stations |

  $station |
  if test("^[0-9]+$") then
    tonumber
  else
    $known_stations[.]
  end
')
cache="/tmp/${LOGNAME}_wetter_$station.png"
curl -sS \
    "http://wetterstationen.meteomedia.de/messnetz/vorhersagegrafik/$station.png" \
    -o "$cache"

if window_id=$(xdotool search --name "^sxiv - $cache$"); then
  xdotool key --window "$window_id" r
else
  sxiv "$cache" &
fi
