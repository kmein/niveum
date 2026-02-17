# Convert and transfer audio files to an MP3 player
{
  writers,
  ffmpeg,
  coreutils,
  gnugrep,
  bash,
}:
writers.writeBashBin "mp3player-write" ''
  set -e

  SPEED=1.0

  while getopts ":s:" opt; do
    case $opt in
      s) SPEED=$OPTARG ;;
      \?) echo "Invalid option: -$OPTARG" >&2; exit 1 ;;
      :) echo "Option -$OPTARG requires a value." >&2; exit 1 ;;
    esac
  done
  shift $((OPTIND -1))

  if [ "$#" -lt 2 ]; then
    echo "Usage: mp3player-write [-s speed] MOUNT_POINT FILE1 [FILE2 ...]"
    exit 1
  fi

  MOUNT_POINT=$1
  shift
  FILES=("$@")

  if [ ! -d "$MOUNT_POINT" ]; then
    echo "Error: Mount point '$MOUNT_POINT' does not exist."
    exit 1
  fi

  TOTAL_SIZE=0
  for f in "''${FILES[@]}"; do
    if [ ! -f "$f" ]; then
      echo "Warning: File '$f' does not exist, skipping."
      continue
    fi
    FILE_SIZE=$(${coreutils}/bin/stat --printf="%s" "$f")
    TOTAL_SIZE=$((TOTAL_SIZE + FILE_SIZE / 2))
  done

  AVAILABLE=$(${coreutils}/bin/df --output=avail "$MOUNT_POINT" | ${coreutils}/bin/tail -n 1)
  AVAILABLE=$((AVAILABLE * 1024))

  if [ "$TOTAL_SIZE" -gt "$AVAILABLE" ]; then
    echo "Error: Not enough space. Required: $TOTAL_SIZE bytes, Available: $AVAILABLE bytes"
    exit 1
  fi

  echo "Enough space available. Starting conversion..."

  sanitize_filename() {
    local name
    name=$(${coreutils}/bin/basename "$1")
    name=''${name%.*}
    name=$(echo "$name" | ${coreutils}/bin/tr ' ' '_' | ${coreutils}/bin/tr -cd '[:alnum:]_-')
    echo "''${name:0:50}"
  }

  for f in "''${FILES[@]}"; do
    [ -f "$f" ] || continue

    existing_prefixes=$(${coreutils}/bin/ls "$MOUNT_POINT" | ${gnugrep}/bin/grep -E '^[0-9].*\.mp3$' | ${coreutils}/bin/sed -E 's/^([0-9]).*/\1/' | ${coreutils}/bin/sort -n | ${coreutils}/bin/uniq)
    for i in {0..9}; do
      if ! echo "$existing_prefixes" | ${gnugrep}/bin/grep -q "^$i$"; then
        PREFIX=$i
        break
      fi
    done

    BASENAME=$(sanitize_filename "$f")
    OUT_PATTERN="$MOUNT_POINT/''${PREFIX}_%03d_''${BASENAME}.mp3"

    echo "Converting '$f' to '$OUT_PATTERN' at speed $SPEED..."

    ${ffmpeg}/bin/ffmpeg -nostdin -i "$f" \
      -filter:a "atempo=$SPEED" \
      -ar 22050 -ac 1 -c:a libmp3lame -b:a 32k \
      -f segment -segment_time 300 \
      "$OUT_PATTERN"
  done

  echo "All files processed successfully."
''
