{
  lib,
  writers,
  yt-dlp,
  miller,
  gnused,
  curl,
  nur,
  downloadDirectory ? "~/mobile/audio/Musik/radiomitschnitt"
}: let
  playlistAPI = "https://radio.lassul.us";

  sendIRC = writers.writeDash "send-irc" ''
    ${nur.repos.mic92.ircsink}/bin/ircsink \
      --nick musikkritiker \
      --server irc.hackint.org \
      --port 6697 \
      --secure \
      --target '#the_playlist' >/dev/null 2>&1
  '';

  messages.good = [
    "what a banger"
    "ooh i love this song"
    "this is top notch stuff!"
    "nice!"
    "noice!"
    "yesss!"
    "cool song!"
    "i like this"
    "that just sounds awesome!"
    "that's a good song!"
    "👍"
    "vibin'"
    "this is a jam!"
    "absolutely love it!"
    "such a vibe!"
    "this track is fire!"
    "feeling this one!"
    "pure gold!"
    "this is my jam!"
    "so catchy!"
    "on repeat!"
    "this is a masterpiece!"
    "let's dance!"
    "I'm all in!"
    "so gut!"
    "mega!"
    "ich fühl's!"
    "das ist Musik für die Seele!"
  ];

  messages.bad = [
    "how can anyone listen to this?"
    "(╯°□°）╯ ┻━┻"
    "skip this!"
    "next, please! i'm suffering!"
    "that's just bad music"
    "nope"
    "that sucks!"
    "👎"
    "turn that down"
    "make it stooop"
    "noooo"
    "this is painful to listen to"
    "what is this noise?"
    "not my cup of tea"
    "this is a hard pass"
    "yikes!"
    "please no more"
    "this is a disaster"
    "I can't handle this"
    "this is just wrong"
    "let's skip this one"
    "ugh, why?"
    "definitely not a fan"
    "wie kann man das hören?"
    "das ist schrecklich!"
    "bitte nicht!"
    "das ist einfach schlecht"
    "oh nein, nicht das!"
  ];

  messages.neutral = [
    "meh"
    "i have no opinion about this song"
    "idk man"
    "it's okay"
    "not bad, not great"
    "just average"
    "it's fine"
    "i can take it or leave it"
    "kann man hören"
    "ist in ordnung"
    "naja"
    "nicht schlecht, nicht gut"
    "ich kann damit leben"
    "es ist was es ist"
  ];

  download = writers.writeDash "download" ''
    ${yt-dlp}/bin/yt-dlp --add-metadata --audio-format mp3 --audio-quality 0 -xic "$@"
  '';
in
  writers.writeDashBin "pls" ''
    case "$1" in
      good|like|cool|nice|noice|top|yup|yass|yes|+)
        response=$(${curl}/bin/curl -sS -XPOST "${playlistAPI}/good")
        echo ${lib.escapeShellArg (lib.concatStringsSep "\n" messages.good)} | shuf -n1 | ${sendIRC}

        # Download the song if a download URL is provided in the string (youtu.be)
        downloadUrl=$(echo "$response" | grep -oE 'https?://(www\.)?(youtube\.com|youtu\.be)/[^\s]+')
        if [ -n "$downloadUrl" ]; then
          echo "Downloading song from URL: $downloadUrl"
          mkdir -p ${lib.escapeShellArg downloadDirectory}
          cd ${lib.escapeShellArg downloadDirectory}
          ${download} "$downloadUrl"
        else
          echo "No download URL found in the response: $response"
        fi
      ;;
      skip|next|bad|sucks|no|nope|flop|-)
        ${curl}/bin/curl -sS -XPOST "${playlistAPI}/skip"
        echo ${lib.escapeShellArg (lib.concatStringsSep "\n" messages.bad)} | shuf -n1 | ${sendIRC}
      ;;
      0|meh|neutral)
        echo ${lib.escapeShellArg (lib.concatStringsSep "\n" messages.neutral)} | shuf -n1 | ${sendIRC}
      ;;
      say|msg)
        shift
        echo "$@" | ${sendIRC}
      ;;
      recent)
        ${curl}/bin/curl -sS -XGET "${playlistAPI}/recent" | tac | head
      ;;
      *)
        ${curl}/bin/curl -sS -XGET "${playlistAPI}/current" \
          | ${miller}/bin/mlr --ijson --oxtab cat \
          | ${gnused}/bin/sed -n '/artist\|title\|youtube/p'
      ;;
    esac
    wait
  ''
