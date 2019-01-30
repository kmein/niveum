{ pkgs, lib, ... }:
let
  theme = import ../../theme.nix;
  unstable = import <nixos-unstable> {};
  bingWallpaper = unstable.writers.writeBash "bing-wallpaper.sh" ''
    PICTURE_DIR="$HOME/pictures/external/bing/"

    mkdir -p $PICTURE_DIR

    urls=( $(${pkgs.curl}/bin/curl -s http://www.bing.com \
        | grep -Eo "url:'.*?'" \
        | sed -e "s/url:'\([^']*\)'.*/http:\/\/bing.com\1/" \
        | sed -e "s/\\\//g") )

    for p in ''${urls[@]}; do
        filename=$(echo $p | sed -e "s/.*\/\(.*\)/\1/")
        if [ ! -f $PICTURE_DIR/$filename ]; then
            ${pkgs.curl}/bin/curl -Lo "$PICTURE_DIR/$filename" $p
        fi
    done
  '';
  scripts.easy-backup = unstable.writers.writeDash "easy-backup" ''
    if [ -d "$1" ]; then
      OUTPUT_ROOT=''${1}/backup/current
      rsync -hav --delete --stats --progress --exclude-from=$HOME/bin/backup.exclude $HOME/* $OUTPUT_ROOT/
    else
      echo "No backup directory supplied"
      exit 1
    fi
  '';
  scripts.pull-all = unstable.writers.writeDashBin "pull-all" ''
    # store the current dir
    CUR_DIR=$(pwd)
    # Let the person running the script know what's going on.
    echo -e "\n\033[1mPulling in latest changes for all repositories...\033[0m\n"
    # Find all git repositories and update it to the master latest revision
    for i in $(find . -name ".git" | cut -c 3-); do
      echo "";
      echo -e "\033[33m"+$i+"\033[0m";
      # We have to go to the .git parent directory to call the pull command
      cd "$i";
      cd ..;
      # finally pull
      git pull origin master;
      # lets get back to the CUR_DIR
      cd $CUR_DIR
    done
    echo -e "\n\033[32mComplete!\033[0m\n"
  '';
  scripts.sp = unstable.writers.writeBashBin "sp" ''
    # This is sp, the command-line Spotify controller. It talks to a running
    # instance of the Spotify Linux client over dbus, providing an interface not
    # unlike mpc.
    #
    # Put differently, it allows you to control Spotify without leaving the comfort
    # of your command line, and without a custom client or Premium subscription.
    #
    # As an added bonus, it also works with ssh, at and cron.
    #
    # Example:
    # $ sp weather girls raining men
    # $ sp current
    # Album   100 Hits Of The '80s
    # Artist  The Weather Girls
    # Title   It's Raining Men
    # $ sp pause
    #
    # Alarm clock example:
    # $ at 7:45 <<< 'sp bangarang'
    #
    # Remote example:
    # $ ssh vader@prod02.nomoon.ta 'sp imperial march'
    #
    #
    # Copyright (C) 2013 Wander Nauta
    #
    # Permission is hereby granted, free of charge, to any person obtaining a copy
    # of this software, to deal in the Software without restriction, including
    # without limitation the rights to use, copy, modify, merge, publish,
    # distribute, sublicense, and/or sell copies of the Software, and to permit
    # persons to whom the Software is furnished to do so, subject to the following
    # conditions:
    #
    # The above copyright notice and this permission notice shall be included in
    # all copies or substantial portions of the Software.
    #
    # The software is provided "as is", without warranty of any kind, express or
    # implied, including but not limited to the warranties of merchantability,
    # fitness for a particular purpose and noninfringement. In no event shall the
    # authors or copyright holders be liable for any claim, damages or other
    # liability, whether in an action of contract, tort or otherwise, arising from,
    # out of or in connection with the software or the use or other dealings in the
    # software.
    #

    # CONSTANTS

    SP_VERSION="0.1"
    SP_DEST="org.mpris.MediaPlayer2.spotify"
    SP_PATH="/org/mpris/MediaPlayer2"
    SP_MEMB="org.mpris.MediaPlayer2.Player"

    # SHELL OPTIONS

    shopt -s expand_aliases

    # UTILITY FUNCTIONS

    function require {
      hash $1 2>/dev/null || {
        echo >&2 "Error: '$1' is required, but was not found."; exit 1;
      }
    }

    # COMMON REQUIRED BINARIES

    # We need dbus-send to talk to Spotify.
    require dbus-send

    # Assert standard Unix utilities are available.
    require grep
    require sed
    require cut
    require tr

    # 'SPECIAL' (NON-DBUS-ALIAS) COMMANDS

    function sp-dbus {
      # Sends the given method to Spotify over dbus.
      dbus-send --print-reply --dest=$SP_DEST $SP_PATH $SP_MEMB.$1 ''${*:2} > /dev/null
    }

    function sp-open {
      # Opens the given spotify: URI in Spotify.
      sp-dbus OpenUri string:$1
    }

    function sp-metadata {
      # Prints the currently playing track in a parseable format.

      dbus-send                                                                   \
      --print-reply                                  `# We need the reply.`       \
      --dest=$SP_DEST                                                             \
      $SP_PATH                                                                    \
      org.freedesktop.DBus.Properties.Get                                         \
      string:"$SP_MEMB" string:'Metadata'                                         \
      | grep -Ev "^method"                           `# Ignore the first line.`   \
      | grep -Eo '("(.*)")|(\b[0-9][a-zA-Z0-9.]*\b)' `# Filter interesting fiels.`\
      | sed -E '2~2 a|'                              `# Mark odd fields.`         \
      | tr -d '\n'                                   `# Remove all newlines.`     \
      | sed -E 's/\|/\n/g'                           `# Restore newlines.`        \
      | sed -E 's/(xesam:)|(mpris:)//'               `# Remove ns prefixes.`      \
      | sed -E 's/^"//'                              `# Strip leading...`         \
      | sed -E 's/"$//'                              `# ...and trailing quotes.`  \
      | sed -E 's/"+/|/'                             `# Regard "" as seperator.`  \
      | sed -E 's/ +/ /g'                            `# Merge consecutive spaces.`
    }

    function sp-current {
      # Prints the currently playing track in a friendly format.
      require column

      sp-metadata \
      | grep --color=never -E "(title)|(album)|(artist)" \
      | sed 's/^\(.\)/\U\1/' \
      | column -t -s'|'
    }

    function sp-eval {
      # Prints the currently playing track as shell variables, ready to be eval'ed
      require sort

      sp-metadata \
      | grep --color=never -E "(title)|(album)|(artist)|(trackid)|(trackNumber)" \
      | sort -r \
      | sed 's/^\([^|]*\)\|/\U\1/' \
      | sed -E 's/\|/="/' \
      | sed -E 's/$/"/' \
      | sed -E 's/^/SPOTIFY_/'
    }

    function sp-art {
      # Prints the artUrl.

      sp-metadata | grep "artUrl" | cut -d'|' -f2
    }

    function sp-display {
      # Calls display on the artUrl.

      require display
      display $(sp-art)
    }

    function sp-feh {
      # Calls feh on the artURl.

      require feh
      feh $(sp-art)
    }

    function sp-url {
      # Prints the HTTP url.

      TRACK=$(sp-metadata | grep "url" | cut -d'|' -f2 | cut -d':' -f3)
      echo "http://open.spotify.com/track/$TRACK"
    }

    function sp-clip {
      # Copies the HTTP url.

      require xclip
      sp-url | xclip
    }

    function sp-http {
      # xdg-opens the HTTP url.

      require xdg-open
      xdg-open $(sp-url)
    }

    function sp-help {
      # Prints usage information.

      echo "Usage: sp [command]"
      echo "Control a running Spotify instance from the command line."
      echo ""
      echo "  sp play       - Play/pause Spotify"
      echo "  sp pause      - Pause Spotify"
      echo "  sp next       - Go to next track"
      echo "  sp prev       - Go to previous track"
      echo ""
      echo "  sp current    - Format the currently playing track"
      echo "  sp metadata   - Dump the current track's metadata"
      echo "  sp eval       - Return the metadata as a shell script"
      echo ""
      echo "  sp art        - Print the URL to the current track's album artwork"
      echo "  sp display    - Display the current album artwork with \`display\`"
      echo "  sp feh        - Display the current album artwork with \`feh\`"
      echo ""
      echo "  sp url        - Print the HTTP URL for the currently playing track"
      echo "  sp clip       - Copy the HTTP URL to the X clipboard"
      echo "  sp http       - Open the HTTP URL in a web browser"
      echo ""
      echo "  sp open <uri> - Open a spotify: uri"
      echo "  sp search <q> - Start playing the best search result for the given query"
      echo ""
      echo "  sp version    - Show version information"
      echo "  sp help       - Show this information"
      echo ""
      echo "Any other argument will start a search (i.e. 'sp foo' will search for foo)."
    }

    function sp-search {
      # Searches for tracks, plays the first result.

      require curl

      Q="$@"
      SPTFY_URI=$( \
        curl -s -G  --data-urlencode "q=$Q" ws.spotify.com/search/1/track \
        | grep -E -o "spotify:track:[a-zA-Z0-9]+" -m 1 \
      )

      sp-open $SPTFY_URI
    }

    function sp-version {
      # Prints version information.

      echo "sp $SP_VERSION"
      echo "Copyright (C) 2013 Wander Nauta"
      echo "License MIT"
    }

    # 'SIMPLE' (DBUS-ALIAS) COMMANDS

    alias sp-play="  sp-dbus PlayPause"
    alias sp-pause=" sp-dbus Pause"
    alias sp-next="  sp-dbus Next"
    alias sp-prev="  sp-dbus Previous"

    # DISPATCHER

    # First, we connect to the dbus session spotify is on. This isn't really needed
    # when running locally, but is crucial when we don't have an X display handy
    # (for instance, when running sp over ssh.)

    SPOTIFY_PID="$(pidof -s spotify)"

    if [[ -z "$SPOTIFY_PID" ]]; then
      echo "Error: Spotify is not running."
      exit 1
    fi

    QUERY_ENVIRON="$(cat /proc/''${SPOTIFY_PID}/environ | tr '\0' '\n' | grep "DBUS_SESSION_BUS_ADDRESS" | cut -d "=" -f 2-)"
    if [[ "''${QUERY_ENVIRON}" != "" ]]; then
      export DBUS_SESSION_BUS_ADDRESS="''${QUERY_ENVIRON}"
    fi

    # Then we dispatch the command.

    subcommand="$1"

    if [[ -z "$subcommand" ]]; then
      # No arguments given, print help.
      sp-help
    else
      # Arguments given, check if it's a command.
      if $(type sp-$subcommand > /dev/null 2> /dev/null); then
        # It is. Run it.
        shift
        eval "sp-$subcommand $@"
      else
        # It's not. Try a search.
        eval "sp-search $@"
      fi
    fi
  '';
  spotifyGenius = unstable.writers.writeDashBin "spgenius.sh" ''
    function normalise {
      echo "$1" | tr ' ' -
    }
    eval $(${scripts.sp} eval)
    ${pkgs.xdg_utils}/bin/xdg-open "http://genius.com/$(normalise "$SPOTIFY_ARTIST")-$(normalise "$SPOTIFY_TITLE")-lyrics"
  '';
  scripts.genius = unstable.writers.writeDashBin "genius" ''
    test $# -eq 2 || (
      echo "usage: $0 <artist> <song>"
      exit 1
    )

    normalize() {
      tr -d -c '0-9A-Za-z ' | tr ' ' - | tr '[:upper:]' '[:lower:]'
    }

    ARTIST=$(echo "$1" | normalize | ${pkgs.gnused}/bin/sed 's/./\U&/')
    TITLE=$(echo "$2" | normalize)
    GENIUS_URL="https://genius.com/$ARTIST-$TITLE-lyrics"

    ${pkgs.curl}/bin/curl -s "$GENIUS_URL" \
      | ${pkgs.gnused}/bin/sed -ne '/class="lyrics"/,/<\/p>/p' \
      | ${pkgs.pandoc}/bin/pandoc -f html -s -t plain \
      | ${pkgs.gnused}/bin/sed 's/^_/\x1b[3m/g;s/_$/\x1b[0m/g;s/^\[/\n\x1b\[1m\[/g;s/\]$/\]\x1b[0m/g'

    printf "\n(source: $GENIUS_URL)\n" >/dev/stderr
  '';
  scripts.generate-shell-nix =
    let generateShellNixPath = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/kmein/generate-shell-nix/81f77661705ee628d1566f2dea01f2d731fda79d/generate-shell-nix";
        sha256 = "0r661z9s5zw0gas2f73aakplfblj1jjlbijmm7gf513xkq61jxm8";
        executable = true;
      };
    in unstable.writers.writeDashBin "generate-shell-nix" ''${generateShellNixPath} $*'';
  scripts.dic =
    let dicPath = pkgs.fetchurl {
      url = "https://cgit.krebsco.de/dic/plain/dic?id=beeca40313f68874e05568f4041423c16202e9da";
      sha256 = "1d25pm420fnbrr273i96syrcd8jkh8qnflpfgxlsbzmbmfizfzld";
      executable = true;
    };
    in unstable.writers.writeDashBin "dic" ''${dicPath} $*'';
  scripts.font-size = unstable.writers.writeDashBin "font-size" ''
    set -efu

    # set_font NORMAL_FONT BOLD_FONT
    set_font() {
      printf '\033]710;%s\007' "$1"
      printf '\033]711;%s\007' "$2"
    }

    case ''${1-} in
      '''|0|--reset)
        set_font \
            "xft:${theme.terminalFont.name}:size=${toString theme.terminalFont.size}" \
            "xft:${theme.terminalFont.name}:size=${toString theme.terminalFont.size}:bold" \
        ;;
      [2-9]|[1-9][0-9]|[1-9][0-9][0-9])
        set_font \
            "xft:${theme.terminalFont.name}:size=$1" \
            "xft:${theme.terminalFont.name}:size=$1:bold" \
        ;;
      *)
        echo "$0: bad argument: $1" >&2
        exit 1
    esac
  '';
  scripts.wttr = unstable.writers.writeDashBin "wttr" ''
    ${pkgs.curl}/bin/curl -s -H "Accept-Language: ''${LANG%_*}" --compressed "wttr.in/''${1-$(${pkgs.curl}/bin/curl -s ipinfo.io | ${pkgs.jq}/bin/jq .loc)}?0"
  '';
  scripts.nix-git = unstable.writers.writeDashBin "nix-git" ''
    ${pkgs.nix-prefetch-git}/bin/nix-prefetch-git "$@" 2> /dev/null | ${pkgs.jq}/bin/jq -r '"rev = \"\(.rev)\";\nsha256 = \"\(.sha256)\";"'
  '';
  scripts.q =
    let
      q-performance = ''
        show_load() {
          ${pkgs.coreutils}/bin/cat /proc/loadavg | ${pkgs.gawk}/bin/gawk '{ print $1, $2, $3}'
        }

        show_memory() {
          ${pkgs.procps}/bin/free -h | ${pkgs.gnugrep}/bin/grep "Mem" | ${pkgs.gawk}/bin/gawk '{print $3}'
        }

        printf "\x1b[33m%s \x1b[1m%s\x1b[0m\n" "$(show_load)" "$(show_memory)"
      '';
      q-isodate = ''
        ${pkgs.coreutils}/bin/date '+[1m%Y-%m-%d[0m [35m%H:%M[0m:%S%:z'
      '';
      q-volume = ''
        is_mute() {
          if $(${pkgs.pamixer}/bin/pamixer --get-mute); then
            echo "(Mute)"
          else
            echo
          fi
        }

        current_volume() {
          ${pkgs.pamixer}/bin/pamixer --get-volume
        }

        printf "\x1b[36m%d%%\x1b[0m %s\n" "$(current_volume)" "$(is_mute)"
      '';
      q-online = ''
        if ${pkgs.curl}/bin/curl -s google.com >/dev/null; then
          echo '[32;1monline[0m'
        else
          echo '[31;1moffline[0m'
        fi
      '';
      q-battery = ''
        BAT_DIR="/sys/class/power_supply/BAT1/"
        if test ! -d "$BAT_DIR"; then
          exit 1
        fi

        if test -e "$BAT_DIR/charge_now" -a -e "$BAT_DIR/charge_full"; then
          FULL_CHARGE="$BAT_DIR/charge_full"
          CURR_CHARGE="$BAT_DIR/charge_now"
        elif test -e "$BAT_DIR/energy_now" -a -e "$BAT_DIR/energy_full"; then
          FULL_CHARGE="$BAT_DIR/energy_full"
          CURR_CHARGE="$BAT_DIR/energy_now"
        else
          ls >&2
          exit 1
        fi

        STATUS="$BAT_DIR/status"

        charge_d=$((100 * $(${pkgs.coreutils}/bin/cat $CURR_CHARGE) / $(${pkgs.coreutils}/bin/cat $FULL_CHARGE)))
        if [[ "$charge_d" -lt 10 ]]; then
          printf "\x1b[31m"
        elif [[ "$charge_d" -lt 20 ]]; then
          printf "\x1b[33m"
        else
          printf "\x1b[32m"
        fi

        printf '%s%%\x1b[0m (%s)\n' "$charge_d" $(${pkgs.coreutils}/bin/cat "$STATUS")
      '';
      q-todo = ''
        TODO_file=$PWD/.todo
        if test -e "$TODO_file"; then
          printf "\n\x1b[1mTodo\x1b[0m\n"
          ${pkgs.coreutils}/bin/cat "$TODO_file" \
            | ${pkgs.gawk}/bin/gawk -v now=$(${pkgs.coreutils}/bin/date +%s) '
                BEGIN { print "remind=0" }
                /^[0-9]/{
                  x = $1
                  gsub(".", "\\\\&", x)
                  rest = substr($0, index($0, " "))
                  rest = $0
                  sub(" *", "", rest)
                  gsub(".", "\\\\&", rest)
                  print "test $(${pkgs.coreutils}/bin/date +%s -d"x") -lt "now" && \
                    echo \"\x1b[93m\""rest esc "\"\x1b[m\" && \
                    (( remind++ ))"
                }
                END { print "test $remind = 0 && echo \"nothing to remind\"" }
              ' \
            | {
              # bash needed for (( ... ))
              ${pkgs.bash}/bin/bash
            }
        fi
      '';
    in unstable.writers.writeBashBin "q" ''
      set -eu
      export PATH=/var/empty
      ${q-isodate}
      ${q-performance}
      (${q-volume}) &
      (${q-battery}) &
      (${q-online}) &
      wait
      ${q-todo}
    '';
in {
  users.users.kfm.packages = lib.attrsets.attrValues scripts;
}
