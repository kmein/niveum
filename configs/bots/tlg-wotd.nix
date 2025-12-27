{
  pkgs,
  config,
  ...
}:
let
  mastodonEndpoint = "https://social.krebsco.de";
in
{
  systemd.services.bot-tlg-wotd = {
    # TODO reenable
    # once https://github.com/NixOS/nixpkgs/pull/462893 is in stable NixOS
    enable = true;
    wants = [ "network-online.target" ];
    startAt = "9:30";
    path = [
      pkgs.jq
      pkgs.curl
      pkgs.recode
      pkgs.deno
      pkgs.imagemagick
      pkgs.gawk
      pkgs.gnugrep
      pkgs.coreutils
    ];
    environment = {
      NPM_CONFIG_CACHE = "/tmp";
      CLTK_DATA = "/tmp";
    };
    script = ''
        set -efux

        chat_id=@tlgwotd

        export TELEGRAM_TOKEN="$(cat "$CREDENTIALS_DIRECTORY/telegram-token")"
        export MASTODON_TOKEN="$(cat "$CREDENTIALS_DIRECTORY/mastodon-token")"

        json_data=$(curl -sSL http://stephanus.tlg.uci.edu/Iris/Wotd | recode html..utf8)

        word=$(echo "$json_data" | jq -r '.word')
        compact_word=$(echo "$word" | sed 's/,.*$//')
        definition=$(echo "$json_data" | jq -r '.definition | sub("<.*>"; "") | rtrimstr(" ")')
        first_occurrence=$(echo "$json_data" | jq -r '.firstOccurrence')
        total_occurrences=$(echo "$json_data" | jq -r '.totalOccurrences')
        telegram_caption="*$word* ‘$definition’

      First occurrence (century): $first_occurrence
      Number of occurrences (in all Ancient Greek texts): $total_occurrences"
        mastodon_caption="$word ‘$definition’

      First occurrence (century): $first_occurrence
      Number of occurrences (in all Ancient Greek texts): $total_occurrences"

        #ancientgreek #classics #wotd #wordoftheday

        transliteration=$(${
          pkgs.writers.writePython3 "translit.py"
            {
              libraries = py: [ py.cltk ];
            }
            ''
              import sys
              from cltk.phonology.grc.transcription import Transcriber

              probert = Transcriber("Attic", "Probert")
              text = " ".join(sys.argv[1:])
              ipa = probert.transcribe(text)

              print(ipa)
            ''
        } "$compact_word")


        photo_path=/tmp/output.png

        hex_to_rgb() {
            hex="$1"
            r=$(printf "%d" "0x$(echo "$hex" | cut -c2-3)")
            g=$(printf "%d" "0x$(echo "$hex" | cut -c4-5)")
            b=$(printf "%d" "0x$(echo "$hex" | cut -c6-7)")
            echo "$r $g $b"
        }

        calculate_luminance() {
            r="$1"
            g="$2"
            b="$3"

            r_l=$(echo "$r" | awk '{print ($1 / 255 <= 0.03928) ? $1 / 255 / 12.92 : (($1 / 255 + 0.055) / 1.055)^2.4}')
            g_l=$(echo "$g" | awk '{print ($1 / 255 <= 0.03928) ? $1 / 255 / 12.92 : (($1 / 255 + 0.055) / 1.055)^2.4}')
            b_l=$(echo "$b" | awk '{print ($1 / 255 <= 0.03928) ? $1 / 255 / 12.92 : (($1 / 255 + 0.055) / 1.055)^2.4}')

            echo "$r_l $g_l $b_l" | awk '{print 0.2126*$1 + 0.7152*$2 + 0.0722*$3}'
        }


        hex_color="#$(echo "$compact_word" | md5sum | cut -c 1-6)"
        if echo "$hex_color" | grep -qE '^#[0-9A-Fa-f]{6}$'; then
            set -- $(hex_to_rgb "$hex_color")
            r="$1"
            g="$2"
            b="$3"
        fi

        luminance=$(calculate_luminance "$r" "$g" "$b")

        threshold="0.1"
        echo "$r $g $b"
        if [ "$(echo "$luminance" | awk -v threshold="$threshold" '{print ($1 > threshold)}')" -eq 1 ]; then
            color1="black"
            color2="#333"
        else
            color1="white"
            color2=lightgrey
        fi

        magick -size 1400x846 \
            xc:"$hex_color" \
            -font "${pkgs.gentium}/share/fonts/truetype/GentiumBookPlus-Bold.ttf" \
            -fill "$color1" \
            -pointsize 150 -gravity west \
            -annotate +100-160 "$compact_word" \
            -font "${pkgs.gentium}/share/fonts/truetype/GentiumBookPlus-Regular.ttf" \
            -fill "$color2" \
            -pointsize 60 -gravity west \
            -annotate +100+00 "$transliteration" \
            -fill "$color1" \
            -annotate +100+120 "‘$definition’" \
            -fill "$color2" \
            -pointsize 40 -gravity southwest \
            -annotate +100+60 "attested $total_occurrences times" \
            -pointsize 40 -gravity southeast \
            -annotate +100+60 "$(date -I)" \
            "$photo_path"

        curl -X POST "https://api.telegram.org/bot$TELEGRAM_TOKEN/sendPhoto" \
             -F "chat_id=\"$chat_id\"" \
             -F "photo=@$photo_path" \
             -F parse_mode=Markdown \
             -F caption="$telegram_caption"

        mastodon_upload_response=$(curl -X POST "${mastodonEndpoint}/api/v2/media" \
            -H "Authorization: Bearer $MASTODON_TOKEN" \
            -F "file=@$photo_path" \
            -F "description=$word ‘$definition’")
        mastodon_image_id=$(echo $mastodon_upload_response | jq -r .id)
        curl -X POST "${mastodonEndpoint}/api/v1/statuses" \
            -H "Authorization: Bearer $MASTODON_TOKEN" \
            -d "status=$mastodon_caption" \
            -d "visibility=public" \
            -d "media_ids[]=$mastodon_image_id"
    '';
    serviceConfig = {
      Type = "oneshot";
      DynamicUser = true;
      StateDirectory = "tlgwotd";
      PrivateTmp = true;
      LoadCredential = [
        "telegram-token:${config.age.secrets.telegram-token-kmein.path}"
        "mastodon-token:${config.age.secrets.mastodon-token-tlgwotd.path}"
      ];
    };
  };

  age.secrets = {
    mastodon-token-tlgwotd.file = ../../secrets/mastodon-token-tlgwotd.age;
  };

  niveum.passport.services = [
    {
      title = "Thesaurus Linguae Graecae Word of the Day";
      description = "sends <a href=\"https://stephanus.tlg.uci.edu/\">TLG</a>'s word of the day to Telegram.";
      link = "https://t.me/tlgwotd";
    }
  ];
}
