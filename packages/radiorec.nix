{
  lib,
  writeShellApplication,
  jq,
  socat,
  yt-dlp,
  dmenu,
  coreutils,
  downloadDirectory ? "/home/kfm/mobile/audio/Musik/radiomitschnitt",
}:
writeShellApplication {
  name = "radiorec";
  runtimeInputs = [ jq socat yt-dlp dmenu coreutils ];
  text = ''
  # We query the mpv IPC server for current metadata
  # In configs/mpv.nix, mpv is wrapped to use --input-ipc-server=/tmp/mpv-$$
  # We find the latest modified mpv IPC socket in /tmp
  # shellcheck disable=SC2012
  SOCKET=$(ls -t /tmp/mpv-* 2>/dev/null | head -n1)
  
  if [ -z "$SOCKET" ] || [ ! -S "$SOCKET" ]; then
    echo "mpv IPC socket not found."
    exit 1
  fi
  
  # Get metadata property
  METADATA=$(echo '{ "command": ["get_property", "metadata"] }' | socat - "$SOCKET" | jq -r '.data')
  
  if [ "$METADATA" = "null" ] || [ -z "$METADATA" ]; then
    echo "No metadata found."
    exit 1
  fi
  
  # Extract Title and Artist (case insensitive handling via jq)
  TITLE=$(echo "$METADATA" | jq -r 'with_entries(.key |= ascii_downcase) | .["icy-title"] // .icy_title // .title // empty')
  ARTIST=$(echo "$METADATA" | jq -r 'with_entries(.key |= ascii_downcase) | .artist // empty')
  
  # If icy_title is present, it often contains "Artist - Title" already. 
  # Otherwise we use "Artist - Title" if both exist, or just Title.
  
  SEARCH_TERM=""
  if [ -n "$TITLE" ]; then
    SEARCH_TERM="$TITLE"
  fi
  if [ -n "$ARTIST" ]; then
    SEARCH_TERM="$ARTIST $SEARCH_TERM"
  fi
  
  if [ -z "$SEARCH_TERM" ]; then
    echo "Could not extract title/artist from metadata."
    echo "$METADATA"
    exit 1
  fi
  
  echo "Searching for: $SEARCH_TERM"
  
  # Search for top 10 results and format them
  RESULTS=$(yt-dlp --print "%(id)s | %(title)s | %(channel)s | %(duration_string)s" "ytsearch10:$SEARCH_TERM" 2>/dev/null)

  if [ -z "$RESULTS" ]; then
    echo "No results found."
    exit 1
  fi
  
  # Ask user to pick one using dmenu
  SELECTED=$(echo "$RESULTS" | dmenu -i -l 10 -p "Download:")
  
  if [ -z "$SELECTED" ]; then
    echo "Aborted."
    exit 0
  fi
  
  # Extract video ID from selection
  VIDEO_ID=$(echo "$SELECTED" | cut -d' ' -f1)

  mkdir -p "${downloadDirectory}"
  cd "${downloadDirectory}"
  
  echo "Downloading video ID: $VIDEO_ID"
  yt-dlp --add-metadata --audio-format mp3 --audio-quality 0 -xic "https://www.youtube.com/watch?v=$VIDEO_ID"
'';
}
