{
  pkgs,
  lib,
  config,
  ...
}: let
  kpaste = pkgs.callPackage <stockholm/krebs/5pkgs/simple/kpaste> {};
  opustags = pkgs.callPackage ../opustags.nix {};
  betacode = pkgs.callPackage ../python3Packages/betacode.nix {};
  wrapScript = {
    packages ? [],
    name,
    script,
  }:
    pkgs.writers.writeDashBin name ''
      PATH=$PATH:${
        lib.makeBinPath (packages ++ [pkgs.coreutils pkgs.findutils])
      }
      ${script} "$@"
    '';
  voidrice = pkgs.fetchFromGitHub {
    owner = "LukeSmithxyz";
    repo = "voidrice";
    rev = "0582b495937117d899ce8ef715a89c6cc25a36cf";
    sha256 = "0c4hkny4zkknlimc9yi9ljss2cws4zn8lzd8ip9b8mfsm094dlfl";
  };
in
  rec {
    auc = pkgs.callPackage ./auc.nix {};

    instaget = wrapScript {
      packages = [pkgs.jq pkgs.curl pkgs.gnugrep];
      script = ./instaget.sh;
      name = "instaget";
    };

    infschmv = pkgs.writers.writeDashBin "InfSchMV" ''
      ${pkgs.curl}/bin/curl -sSL https://www.berlin.de/corona/massnahmen/verordnung/ \
        | ${pkgs.pup}/bin/pup .textile \
        | ${pkgs.pandoc}/bin/pandoc -f html -t man -s \
        | ${pkgs.man}/bin/man -l -
    '';

    trans = let
      script = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/soimort/translate-shell/gh-pages/trans.awk";
        sha256 = "178r8d27bry1mzd1g8x2svp4w469hwv7nnxnmnsinx974skjx0jb";
      };
    in
      pkgs.writers.writeDashBin "trans" ''
        ${pkgs.gawk}/bin/gawk -f ${script} -- "$@"
      '';

    dns-sledgehammer = pkgs.writers.writeDashBin "dns-sledgehammer" ''
      ${pkgs.coreutils}/bin/printf '%s\n' 'nameserver 1.1.1.1' 'options edns0' > /etc/resolv.conf
    '';

    showkeys-toggle = pkgs.writers.writeDashBin "showkeys-toggle" ''
      if ${pkgs.procps}/bin/pgrep screenkey; then
        exec ${pkgs.procps}/bin/pkill screenkey
      else
        exec ${pkgs.screenkey}/bin/screenkey
      fi
    '';

    qrpaste = pkgs.writers.writeDashBin "qrpaste" ''
      file="$(${pkgs.mktemp}/bin/mktemp --tmpdir)"
      trap clean EXIT
      clean() {
        rm "$file"
      }
      ${pkgs.qrencode}/bin/qrencode "$(${pkgs.xclip}/bin/xclip -selection clipboard -out)" -o "$file"
      ${pkgs.nsxiv}/bin/nsxiv "$file"
    '';

    ttspaste = pkgs.writers.writeDashBin "ttspaste" ''
      ${pkgs.xclip}/bin/xclip -selection clipboard -out | ${pkgs.curl}/bin/curl -G http://tts.r/api/tts --data-urlencode 'text@-' | ${pkgs.mpv}/bin/mpv -
    '';

    interdimensional-cable = let
      nimaid-github-io = pkgs.fetchFromGitHub {
        owner = "nimaid";
        repo = "nimaid.github.io";
        rev = "9cb4ede215be6bb01bd2df1ef3e9689cc8c4eb9e";
        sha256 = "1g47cj5an7xgmhpc09m7qim5j9rspqxvnzfy90cnlvz4pg8hil96";
      };
    in
      pkgs.writeShellScriptBin "interdimensional-cable" ''
        export PATH=${lib.makeBinPath [pkgs.mpv pkgs.jq pkgs.gnused]}
        mpv --shuffle --playlist=<(jq -r '.videos[]' ${nimaid-github-io}/tv/interdimensional_database.json | sed 's#^#https://youtu.be/#')
      '';

    tag = wrapScript {
      packages = [pkgs.vorbis-tools pkgs.python3Packages.eyeD3 opustags];
      script = "${voidrice}/.local/bin/tag";
      name = "tag";
    };

    meteo = wrapScript {
      packages = [pkgs.jq pkgs.curl pkgs.xdotool pkgs.nsxiv pkgs.gnused];
      script = ./meteo.sh;
      name = "meteo";
    };

    kirciuoklis = wrapScript {
      packages = [pkgs.curl pkgs.jq];
      script = ./kirciuoklis.sh;
      name = "kirciuoklis";
    };

    booksplit = wrapScript {
      packages = [pkgs.ffmpeg tag pkgs.glibc.bin];
      script = "${voidrice}/.local/bin/booksplit";
      name = "booksplit";
    };

    n = wrapScript {
      script = ./n.sh;
      name = "n";
    };

    dirmir = wrapScript {
      name = "dirmir";
      script = ./dirmir.sh;
    };

    liddel-scott-jones = wrapScript {
      name = "lsj";
      script = ./liddel-scott-jones.sh;
      packages = [pkgs.curl pkgs.pup betacode pkgs.gnused pkgs.pandoc pkgs.man];
    };

    sanskrit-dictionary = pkgs.writers.writeDashBin "sa" ''
      set -efu

      usage() {
        echo "usage: [OUTPUT=deva|roman] $0 mw|mwe|boet|bopp|apte|boro TERM"
        exit 1
      }

      [ $# -eq 2 ] || usage

      case $1 in
        mw) id=MWScan;;
        mwe) id=MWEScan;;
        bopp) id=BOPScan;;
        boet) id=PWGScan;;
        apte) id=AEScan;;
        boro) id=BORScan;;
        *) usage;;
      esac

      shift

      input="$*"

      ${pkgs.curl}/bin/curl -sSL "https://www.sanskrit-lexicon.uni-koeln.de/scans/$id/2020/web/webtc/getword.php?key=$input&filter=''${OUTPUT-roman}&accent=yes&transLit=hk" \
        | ${pkgs.pandoc}/bin/pandoc --standalone --variable=title:"$input" --from=html --to=man \
        | ${pkgs.gnused}/bin/sed 's/\s\+\([:.,;]\)/\1/g;s/\s\+/ /g' \
        | ${pkgs.man}/bin/man --local-file --pager="${pkgs.bat}/bin/bat -p" -
    '';

    playlist = import ./pls.nix {inherit pkgs lib config;};

    mpv-tv = import ./mpv-tv.nix {inherit pkgs lib;};

    favicon = wrapScript {
      packages = [pkgs.imagemagick];
      name = "favicon";
      script = ./favicon.sh;
    };

    closest = pkgs.writers.writeDashBin "closest" ''
      ${
        pkgs.writers.writeHaskellBin "closest" {
          libraries = with pkgs.haskellPackages; [parallel optparse-applicative edit-distance];
          ghcArgs = ["-O3" "-threaded"];
        } (builtins.readFile ./distance.hs)
      }/bin/closest +RTS -N4 -RTS --dictionary ${
        pkgs.fetchurl {
          url = "https://gist.github.com/MarvinJWendt/2f4f4154b8ae218600eb091a5706b5f4/raw/36b70dd6be330aa61cd4d4cdfda6234dcb0b8784/wordlist-german.txt";
          sha256 = "0vr4lmlckgvj4s8sk502sknq9pf3297rvasj5sqqm05zzbdgpppj";
        }
      } "$@"
    '';

    horoscope = pkgs.callPackage ./horoscope {};

    genius = wrapScript {
      packages = [pkgs.curl pkgs.gnused pkgs.pandoc];
      name = "genius";
      script = ./genius.sh;
    };

    literature-quote = wrapScript {
      packages = [pkgs.xsv pkgs.curl pkgs.gnused];
      name = "literature-quote";
      script = ./literature-quote.sh;
    };

    man-pdf = wrapScript {
      packages = [pkgs.man pkgs.ghostscript];
      name = "man-pdf";
      script = ./man-pdf.sh;
    };

    odyssey = wrapScript {
      packages = [pkgs.curl pkgs.xmlstarlet];
      name = "odyssey";
      script = ./odyssey.sh;
    };

    tolino-screensaver = wrapScript {
      packages = [pkgs.imagemagick];
      name = "tolino-screensaver";
      script = ./tolino-screensaver.sh;
    };

    wttr = wrapScript {
      packages = [pkgs.curl];
      name = "wttr";
      script = ./wttr.sh;
    };

    vg = wrapScript {
      packages = [pkgs.ripgrep pkgs.fzf pkgs.gawk];
      name = "vg";
      script = ./vg.sh;
    };

    dmenurandr = wrapScript {
      packages = [pkgs.dmenu pkgs.gnugrep pkgs.gnused pkgs.xorg.xrandr pkgs.gawk pkgs.libnotify pkgs.arandr];
      name = "dmenurandr";
      script = ./dmenurandr.sh;
    };

    dmenubluetooth = wrapScript {
      packages = [pkgs.bluez5 pkgs.dmenu pkgs.libnotify];
      name = "dmenubluetooth";
      script = ./dmenubluetooth.sh;
    };

    fkill = wrapScript {
      packages = [pkgs.procps pkgs.gawk pkgs.gnused pkgs.fzf pkgs.bash];
      script = ./fkill.sh;
      name = "fkill";
    };

    nix-git = wrapScript {
      packages = [pkgs.nix-prefetch-git pkgs.jq];
      script = ./nix-git.sh;
      name = "nix-git";
    };

    mansplain = wrapScript {
      packages = [pkgs.man pkgs.zathura pkgs.dmenu pkgs.gnused];
      script = ./mansplain.sh;
      name = "mansplain";
    };

    notetags = wrapScript {
      script = ./notetags.sh;
      name = "notetags";
    };

    fzfmenu = wrapScript {
      script = ./fzfmenu.sh;
      name = "fzfmenu";
      packages = [pkgs.st pkgs.fzf pkgs.dash pkgs.bash];
    };

    swallow = wrapScript {
      script = ./swallow.sh;
      name = "swallow";
      packages = [pkgs.xdo];
    };

    ipa = wrapScript {
      script = ./ipa.py;
      name = "ipa";
      packages = [pkgs.python3];
    };

    default-gateway = pkgs.writers.writeDashBin "default-gateway" ''
      ${pkgs.iproute2}/bin/ip -json route | ${pkgs.jq}/bin/jq --raw-output '.[0].gateway'
    '';

    betacode =
      pkgs.writers.writeHaskellBin "betacode" {
        libraries = with pkgs; [
          (haskell.lib.unmarkBroken (haskell.lib.doJailbreak haskellPackages.betacode))
          haskellPackages.text
        ];
      } ''
        import qualified Data.Text.IO as T
        import qualified Data.Text as T
        import Text.BetaCode
        main = T.interact (either (error . T.unpack) id . fromBeta)
      '';

    devanagari = pkgs.callPackage ../devanagari {};

    timer = pkgs.writers.writeDashBin "timer" ''
      [ $# -eq 2 ] || {
        echo "Usage: $0 TIME MESSAGE" 1>&2
        exit 1
      }
      time=$(echo "$1" | ${pkgs.bc}/bin/bc)
      echo "sleeping $time seconds, then saying: $2"
      ${pkgs.coreutils}/bin/sleep "$time" && {
        echo "$2" | ${pkgs.espeak}/bin/espeak -v german-mbrola-6
      }
    '';

    manual-sort = pkgs.writers.writeHaskellBin "manual-sort" {} ''
      {-# LANGUAGE LambdaCase #-}
      import Data.Char (toLower)
      import System.Environment (getArgs)
      import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

      insertionSortM :: Monad f => (a -> a -> f Ordering) -> [a] -> f [a]
      insertionSortM cmp = foldr ((=<<) . insertByM cmp) (pure [])
       where
        insertByM cmp x = \case
          [] -> pure [x]
          yys@(y : ys) -> cmp x y >>= \case
            GT -> (y :) <$> insertByM cmp x ys
            _ -> pure (x : yys)

      ask :: Show a => a -> a -> IO Ordering
      ask a b = do
        putStr (show a ++ " > " ++ show b ++ "? (y/n) ")
        map toLower <$> getLine >>= \case
          'y' : _ -> return GT
          _ -> return LT

      main :: IO ()
      main = do
        hSetBuffering stdout NoBuffering
        argv <- getArgs
        sorted <- insertionSortM ask argv
        mapM_ (\(place, thing) -> putStrLn (show place ++ ". " ++ show thing)) $ zip [1 ..] (reverse sorted)
    '';

    scrot-dmenu = wrapScript {
      script = ./scrot-dmenu.sh;
      name = "dmenu-scrot";
      packages = [pkgs.xclip pkgs.scrot kpaste pkgs.libnotify pkgs.dmenu];
    };

    unicodmenu = pkgs.callPackage ./unicodmenu.nix {};

    mpv-radio = let
      streams = import ../../lib/streams.nix {
        di-fm-key = "%DI_FM_KEY%"; # lib.strings.fileContents <secrets/di.fm/key>;
      };
      streams-tsv = pkgs.writeText "streams.tsv" (lib.concatMapStringsSep "\n" ({
        desc ? "",
        stream,
        station,
        ...
      }: "${station}\t${desc}\t${stream}")
      streams);
    in
      pkgs.writers.writeDashBin "mpv-radio" ''
        export DI_FM_KEY=$(cat "${config.age.secrets.di-fm-key.path}")
        exec ${pkgs.mpv}/bin/mpv --force-window=yes "$(
          ${pkgs.dmenu}/bin/dmenu -i -l 5 < ${streams-tsv} \
            | ${pkgs.coreutils}/bin/cut -f3 \
            | ${pkgs.gnused}/bin/sed s/%DI_FM_KEY%/"$DI_FM_KEY"/
        )"
      '';

    rfc = wrapScript {
      script = ./rfc.sh;
      name = "rfc";
      packages = [pkgs.curl pkgs.pup pkgs.gnused pkgs.gawk pkgs.gnugrep pkgs.fzf];
    };

    new-mac = wrapScript {
      script = ./new-mac.sh;
      name = "new-mac";
      packages = [pkgs.gnused pkgs.openssl pkgs.jq pkgs.iproute2];
    };

    bvg = pkgs.callPackage ./bvg.nix {};
    nav = pkgs.callPackage ./nav.nix {};
    k-lock = pkgs.callPackage ./k-lock.nix {};

    menu-calc = pkgs.writers.writeDashBin "=" ''
      # https://github.com/onespaceman/menu-calc

      answer=$(echo "$@" | ${pkgs.pari}/bin/gp -q | ${pkgs.gnused}/bin/sed '/\./ s/\.\{0,1\}0\{1,\}$//')

      action=$(printf "copy\nclear" | ${pkgs.dmenu}/bin/dmenu -p "= $answer")

      case $action in
        "clear") $0 ;;
        "copy") printf %s "$answer" | ${pkgs.xclip}/bin/xclip -selection clipboard;;
        "") ;;
        *) $0 "$answer $action" ;;
      esac
    '';

    # ref https://tex.stackexchange.com/a/502542
    scanned = pkgs.writers.writeDashBin "scanned" ''
      [ $# -eq 1 -a -f "$1" -a -r "$1" ] || exit 1

      ${pkgs.imagemagick}/bin/convert -density 150 "$1" -rotate 0.5 -attenuate 0.25 +noise Multiplicative -colorspace Gray "scanned-$1"
    '';

    nix-index-update = pkgs.writers.writeDashBin "nix-index-update" ''
      filename="index-x86_64-$(uname | tr A-Z a-z)"
      mkdir -p ~/.cache/nix-index
      cd ~/.cache/nix-index
      # -N will only download a new version if there is an update.
      ${pkgs.wget}/bin/wget -q -N https://github.com/Mic92/nix-index-database/releases/latest/download/$filename
      ln -f $filename files
    '';

    heuretes = let
      database = pkgs.fetchurl {
        url = "http://c.krebsco.de/greek.csv";
        hash = "sha256-SYL10kerNI0HzExG6JXh765+CBBCHLO95B6OKErQ/sU=";
      };
    in
      pkgs.writers.writeDashBin "heuretes" ''
        ${pkgs.xsv}/bin/xsv search -s simple "^$*$" ${database} | ${pkgs.xsv}/bin/xsv table
      '';

    # https://nitter.net/igor_chubin/status/1557793569104183298
    stackoverflow = pkgs.writers.writeDashBin "so" ''
      IFS=+
      ${pkgs.curl}/bin/curl -sSL http://cht.sh/"$*"
    '';

    rofi-hass = pkgs.writers.writeBashBin "rofi-hass" ''
      export PATH=${lib.makeBinPath [pkgs.home-assistant-cli pkgs.jq pkgs.util-linux pkgs.rofi pkgs.gnused pkgs.libnotify]}
      json=$(hass-cli -o json state list 2>/dev/null)
      idx=$(jq -r '.[] | [.entity_id, .state] | join(" ")' <<< "$json" | column -t | rofi -dmenu -i -markup-rows -format d)
      item=$(jq -r '.[].entity_id' <<< "$json" | sed "''${idx}q;d")
      itype=$(sed -r 's/\..+$//' <<< "$item")

      case "$itype" in
        light) hass-cli state toggle "$item" &>/dev/null ;;
        scene) hass-cli service call --arguments entity_id="$item" scene.turn_on &>/dev/null ;;
        *) notify-send "Error" "Event type '$itype' not implemented yet. Do you have time to file an issue or write a PR?" ;;
      esac
    '';
  }
  // {
    devour = pkgs.callPackage ../devour.nix {};
    depp = pkgs.callPackage ../depp.nix {};
    text2pdf = pkgs.callPackage ../text2pdf.nix {};
    vimv = pkgs.callPackage ../vimv.nix {};
    when = pkgs.callPackage ../when.nix {};
    mahlzeit = pkgs.haskellPackages.callPackage ../mahlzeit.nix {};
    inherit opustags;
  }
