{
  writers,
  flite,
  netcat,
  gnused,
  ...
}:
writers.writeDashBin "brainmelter" ''
  SERVER="brockman.news"
  PORT=6667
  NICK="irccat"
  USER="irccat"
  CHANNEL="#all"

  # Open connection to IRC server using a FIFO and netcat
  FIFO=$(mktemp -u)
  mkfifo "$FIFO"
  trap "rm -f $FIFO" EXIT

  # Send IRC commands
  {
      echo "NICK $NICK"
      echo "USER $USER 0 * :$USER"
      sleep 5
      echo "JOIN $CHANNEL"
      while true; do
          sleep 30
          echo "PING :keepalive"
      done
  } > "$FIFO" &

  # Read from server and write to stdout
  ${netcat}/bin/nc "$SERVER" "$PORT" < "$FIFO" | while IFS= read -r line; do
    voice="$(echo -e "awb\nkal\nrms\nslt" | shuf -n1)"
    echo "$line" \
      | ${gnused}/bin/sed -n 's/.*go.brockman.news\/\S\+ //p' \
      | tee /dev/stderr \
      | ${flite}/bin/flite -voice "$voice"
    # Respond to PINGs to avoid timeout
    if [ "$line" = PING* ]; then
        server_ping=$(echo "$line" | cut -d':' -f2)
        echo "PONG :$server_ping" > "$FIFO"
    fi
  done
''
