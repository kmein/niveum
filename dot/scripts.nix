pkgs: defaultApplications:
let
  bingWallpaper = pkgs.writeBash "bing-wallpaper.sh" ''
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
  colorize = pkgs.writeBash "colorize.sh" ''
    highlight=$(which highlight 2>/dev/null)
    pygmentize=$(which pygmentize 2>/dev/null)

    colorize() {
      if [ "$TERM" == "dumb" -o "$NO_COLORS" == "1" ]
      then
        cat
      else
        if [ "$1" == "haskell" ]; then
          ${pkgs.haskellPackages.hscolour}/bin/HsColour -tty
        else
          ${pkgs.python36Packages.pygments}/bin/pygmentize -l $1 -O bg=dark
        fi
      fi
    }

    if [ -z "$2" ]; then
      colorize "$1"
    else
      cat "$2" | colorize "$1"
    fi
  '';
  easyBackup = pkgs.writeBash "easy-backup.sh" ''
    if [ -d "$1" ]; then
      OUTPUT_ROOT=''${1}/backup/current
      rsync -hav --delete --stats --progress --exclude-from=$HOME/bin/backup.exclude $HOME/* $OUTPUT_ROOT/
    else
      echo "No backup directory supplied"
      exit 1
    fi
  '';
  compile = pkgs.writeBash "compile.sh" ''
    if [ -z "$1" ]; then
      echo "Usage: compile <source file>"
    fi

    file="$1"
    ext="''${file##*.}"
    name="''${file%.*}"

    case "$ext" in
      asm)
        echo Compiling "$name" with the Netwide Assembler...
        nasm -felf64 -o "$name".o "$file"
        gcc -o "$name" "$name".o;;
      c)
        echo Compiling "$name" with the GNU C Compiler...
        gcc -std=c11 -Wall -Wpedantic -Ofast -o "$name" "$file";;
      cpp|cxx|cc)
        echo Compiling "$name" with the GNU C++ Compiler...
        g++ -std=c++11 -Wall -O3 -o "$name" "$file";;
      bf)
        echo Compiling "$name" with the BrainFuck Compiler...
        bf2c "$file" "$name".c && compile "$name".c && rm "$name".c;;
      s)
        echo Assembling "$name" with the GNU Assembler...
        gcc -o "$name" "$file";;
      hs)
        echo Compiling "$name" with the Glasgow Haskell Compiler...
        stack ghc -- -Wall -O3 --make "$file" && rm "$name".hi "$name".o;;
      java)
        echo Compiling "$name" with the Oracle Java Compiler...
        javac "$file";;
      rb)
        echo Compiling "$name" with Ruby...
        ruby -c "$file";;
      lua)
        echo Compiling "$name" with the Lua Compiler...
        luac -o "$name".luac "$file";;
      py)
        if [ ""$2"" = "--optimize" ]; then
          echo Compiling "$name" with the optimizing Python Compiler...
          if [ -e "$name".pyo ]; then
            rm "$name".pyo
          fi
          python3 -O -mpy_compile "$file"
          mv __pycache__/"$name".cpython-33.pyo ./"$name".pyo
          rmdir __pycache__
        else
          echo Compiling "$name" with the Python Compiler...
          if [ -e "$name".pyc ]; then
            rm "$name".pyc
          fi
          python3 -mpy_compile "$file"
          mv __pycache__/"$name".cpython-33.pyc ./"$name".pyc
          rmdir __pycache__
        fi;;
      ml)
        echo Compiling "$name" with the OCaml Compiler...
        ocamlc -o "$name" "$file" && rm "$name".cmo "$name".cmi;;
      cs)
        echo Compiling "$name" with the Mono C£ Compiler...
        mono-csc -out:"$name" "$file";;
      fs)
        echo Compiling "$name" with the F£ Compiler...
        fsharpc -out:"$name" "$file";;
      vala)
        echo Compiling "$name" with the Vala Compiler...
        valac --pkg=gtk+-3.0 "$file";;
      md)
        echo Converting "$name" to PDF with Pandoc...
        pandoc --latex-engine=xelatex -V documentclass=scrartcl -o "$name".pdf "$file";;
      tex)
        echo Compiling "$name" to PDF with LaTeX...
        latexmk -pdf "$file";;
      lhs)
        echo Compiling "$name" with the Glasgow Haskell Compiler...
        ghc -Wall -O3 --make "$file"
        echo Converting "$name" to PDF Markup with Pandoc...
        pandoc "$file" -f markdown+lhs --latex-engine=xelatex -V documentclass=scrartcl -o "$name".pdf;;
      d)
        echo Compiling "$name" with the GNU D Compiler...
        gdc -Wall -O3 -o "$name" "$file";;
      m)
        echo Compiling "$name" with the GNU/GNUstep Objective C Compiler...
        £ gcc -Wall -o "$name" -MMD -MP -DGNUSTEP -DGNUSTEP_BASE_LIBRARY=1 -DGNU_GUI_LIBRARY=1 -DGNU_RUNTIME=1 -DGNUSTEP_BASE_LIBRARY=1 -fno-strict-aliasing -fexceptions -fobjc-exceptions -D_NATIVE_OBJC_EXCEPTIONS -pthread -fPIC -DGSWARN -Wno-import -g -O2 -fgnu-runtime -fconstant-string-class=NSConstantString -fexec-charset=UTF-8 -I. -I/usr/local/include/GNUstep -I/usr/include/GNUstep -lobjc -lgnustep-base "$file"
        gcc `gnustep-config --objc-flags` "$file" -o "$name" -lgnustep-base -lobjc
        ;;
      pas)
        echo Compiling "$name" with the FreePascal Compiler...
        fpc "$file";;
      rs)
        echo Compiling "$name" with the Rust Compiler...
        rustc "$file";;
      ts)
        echo Compiling "$name" with the TypeScript Compiler...
        tsc "$file";;
      *)
        echo Compiler for "$ext" not found!;;
    esac
  '';
  gitPullAll = pkgs.writeBash "git-pull-all.sh" ''
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
  gripe = pkgs.writeBash "gripe.sh" ''${sidepipe} '(:\d+:|-\d+-|--)(\x1b[[]K)?' "$@"'';
  haskellDefinition = pkgs.writeBash "hdef.sh" ''
    paths=""
    while true; do
        if [ -d "$1" ]; then
            paths="$paths $1"
        else
            break
        fi
        shift
    done
    str="$1"
    shift

    lower=$(echo "$str" | tr A-Z a-z)
    if [ "''${lower:0:1}" == "''${str:0:1}" ]; then
        expr="($str( |$)|[[:space:]]+$str[[:space:]]*::)"
    else
        kws="(class|data|type|newtype)"
        eow="([ \n\t]|$)"
        expr="$kws[[:space:]]+($str$eow|[^=]+=>[[:space:]]+$str$eow)"
    fi

    ${haskellFind} $paths -print0 | xargs -0 grep -En --colour=never -A10 "$@" "^$expr" | ${gripe} hcol | ${highlight} $str
  '';
  haskellFind = pkgs.writeBash "hfind.sh" ''
    paths=""
    while true; do
        if [ -d "$1" ]; then
            paths="$paths $1"
        else
            break
        fi
        shift
    done
    find $paths \( -name "*.hs" -or -name "*.hsi" -or -name "*.lhs" -or -name "*.hs-boot" \) -a -not \( -name ".*" -or -path "*/_darcs/*" -o -name '.£*' \) "$@"
  '';
  haskellGrep = pkgs.writeBash "hgrep.sh" ''
    if [ -z "$1" -o "$1" == "--help" -o "$1" == "-h" ]; then
      echo "Usage: hg [PATH] IDENTIFIER [GREP OPTIONS...]"
      echo "Seaches for uses of the given Haskell identifier."
      exit 1
    fi

    paths=""
    while true; do
        if [ -d "$1" ]; then
            paths="$paths $1"
        else
            break
        fi
        shift
    done

    colour=always
    if [ "$TERM" == "dumb" -o "$NO_COLORS" == "1" ]; then
        colour=never
    fi
    ${haskellFind} $paths -print0 | xargs -0 grep -nw --colour=$colour "$@"
  '';
  highlight = pkgs.writePython3 "hl.py" {
    deps = [ pkgs.python36Packages.ansicolors ];
    flakeIgnore = [ "E302" "E999" "E231" "E701" "W605" "E231" "E305" ];
    } ''
    import os
    import sys
    import re
    from ansicolors import *

    def replfun((re,colfun)):
        def onmatch(m):
            if m.groups() != ():
                buf = []
                p = m.start(0)
                mstr = m.string
                try:
                    i = 1
                    while True:
                        (s,e) = m.span(i)
                        if s == -1: continue
                        buf.append(mstr[p:s])
                        buf.append(colfun(mstr[s:e]))
                        p = e
                        i += 1
                except IndexError:
                    buf.append(mstr[p:m.end(0)])
                    return '\'.join(buf)
            else:
                return colfun(m.expand("\g<0>"))
        return lambda s: re.subn(onmatch,s)[0]

    colfuns = [green, red, magenta, cyan, blue]
    flags = re.L
    if len(sys.argv) > 1 and sys.argv[1] == '-i':
        flags = flags | re.I
        exps = sys.argv[2:]
    else:
        exps = sys.argv[1:]
    regexps = map(lambda e: re.compile(e, flags), exps)
    hlfuns = map(replfun, zip(regexps, colfuns))

    term = os.getenv("TERM")
    no_colors = os.getenv("NO_COLORS")
    if term == "dumb" or no_colors == "1":
        for line in sys.stdin:
            sys.stdout.write(line)
    else:
        for line in sys.stdin:
            sys.stdout.write(reduce(lambda s, f: f(s), hlfuns, line))
  '';
  haskellTags = pkgs.writeBash "htags.sh" ''
    id="[a-z_][a-zA-Z0-9_\']*"
    ws="[ \\t]"
    ID="[A-Z][a-zA-Z0-9_\']*"

    ${pkgs.ctags}/bin/ctags --tag-relative=no \
      '--langdef=haskell' \
      '--langmap=haskell:.hs.lhs' \
      '--regex-haskell=/^(type|data|newtype)[ \t]+([^ \t=]+)/\2/' \
      '--regex-haskell=/^class[^=]+=>[ \t]*([^ \t]+)/\1/' \
      '--regex-haskell=/^class[ \t]+([^ \t]+)[^=]*$/\1/' \
      "--regex-haskell=/^$ws*($id)$ws*::/\1/" \
      "--regex-haskell=/^($id)/\1/" \
      "$@"
  '';
  sidepipe = pkgs.writePython3 "sidepipe.py" { flakeIgnore = [ "E302" "E231" "E999" "E265" "E305" ]; } ''
    import sys
    import re
    import os.path
    from Queue import Queue
    from subprocess import Popen, PIPE
    from threading import Thread

    pipeIsLeft = os.path.basename(sys.argv[0]) == "leftpipe"
    regex = sys.argv[1]
    cmd = sys.argv[2:]

    class WorkerThread(Thread):
        def __init__(self, queue, instream, outstream, pipeIsLeft):
            Thread.__init__(self)
            self.queue = queue
            self.instream = instream
            self.outstream = outstream
            self.pipeIsLeft = pipeIsLeft

    class SplitThread(WorkerThread):
        def run(self):
            try:
                for line in self.instream:
                    match = re.search(regex,line)
                    if not match:
                        print >> sys.stderr, "No match: %r in %r" % (regex,line)
                        continue
                    index = match.end()
                    left, right = line[:index], line[index:]
                    if pipeIsLeft:
                        self.outstream.write(left)
                        self.queue.put(right)
                    else:
                        self.outstream.write(right)
                        self.queue.put(left)
                self.outstream.close()
            except IOError, e:
                print >> sys.stderr, e[1]
                self.outstream.close()
            while not queue.full():
                queue.put("")

    class JoinThread(WorkerThread):
        def run(self):
            for pipeline in self.instream:
                #print >> sys.stderr, "wait"
                passline = self.queue.get()
                #print >> sys.stderr, "ok"
                if pipeIsLeft:
                    self.outstream.write(pipeline)
                    self.outstream.write(passline)
                else:
                    self.outstream.write(passline)
                    self.outstream.write(pipeline)

    pipe = Popen(cmd, shell=False,stdin=PIPE,stdout=PIPE)
    queue = Queue(250)
    t1 = SplitThread(queue, sys.stdin, pipe.stdin, pipeIsLeft)
    t2 = JoinThread(queue, pipe.stdout, sys.stdout, pipeIsLeft)
    t1.start()
    t2.start()
    t2.join()
  '';
  spotifyCli = pkgs.writeBash "sp.sh" ''
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
  spotifyGenius = pkgs.writeBash "spgenius.sh" ''
    function normalise {
      echo ''${1// /-}
    }
    eval $(${spotifyCli} eval)
    ${defaultApplications.webBrowser} "http://genius.com/$(normalise "$SPOTIFY_ARTIST")-$(normalise "$SPOTIFY_TITLE")-lyrics"
  '';
in {
  compile = compile;
  easy-backup = easyBackup;
  colorize = colorize;
  bing-wallpaper = bingWallpaper;
  git-pull-all = gitPullAll;
  hdef = haskellDefinition;
  hfind = haskellFind;
  hgrep = haskellGrep;
  htags = haskellTags;
  sp = spotifyCli;
  spgenius = spotifyGenius;
}
