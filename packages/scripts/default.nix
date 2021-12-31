{ pkgs, lib, ... }:
let
  kpaste = pkgs.callPackage <stockholm/krebs/5pkgs/simple/kpaste> { };
  opustags = pkgs.callPackage <niveum/packages/opustags.nix> { };
  betacode = pkgs.callPackage <niveum/packages/python3Packages/betacode.nix> { };
  wrapScript = { packages ? [ ], name, script }:
    pkgs.writers.writeDashBin name ''
      PATH=$PATH:${
        lib.makeBinPath (packages ++ [ pkgs.coreutils pkgs.findutils ])
      }
      ${script} "$@"
    '';
  voidrice = pkgs.fetchFromGitHub {
    owner = "LukeSmithxyz";
    repo = "voidrice";
    rev = "0582b495937117d899ce8ef715a89c6cc25a36cf";
    sha256 = "0c4hkny4zkknlimc9yi9ljss2cws4zn8lzd8ip9b8mfsm094dlfl";
  };
in rec {
  auc = pkgs.callPackage ./auc.nix {};

  instaget = wrapScript {
    packages = [ pkgs.jq pkgs.curl pkgs.gnugrep ];
    script = ./instaget.sh;
    name = "instaget";
  };

  infschmv = pkgs.writers.writeDashBin "InfSchMV" ''
    ${pkgs.curl}/bin/curl -sSL https://www.berlin.de/corona/massnahmen/verordnung/ \
      | ${pkgs.pup}/bin/pup .textile \
      | ${pkgs.pandoc}/bin/pandoc -f html -t man -s \
      | ${pkgs.man}/bin/man -l -
  '';

  trans =
    let
      script = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/soimort/translate-shell/gh-pages/trans.awk";
        sha256 = "178r8d27bry1mzd1g8x2svp4w469hwv7nnxnmnsinx974skjx0jb";
      };
    in pkgs.writers.writeDashBin "trans" ''
      ${pkgs.gawk}/bin/gawk -f ${script} -- "$@"
    '';

  dns-sledgehammer = pkgs.writers.writeDashBin "dns-sledgehammer" ''
    ${pkgs.coreutils}/bin/printf '%s\n' 'nameserver 1.1.1.1' 'options edns0' > /etc/resolv.conf
  '';

  much-scripts = let
    much-current-query = wrapScript {
      packages = [ pkgs.curl ];
      name = "much-current-query";
      script = ./much-current-query.sh;
    };
    mail-send = wrapScript {
      packages = [ pkgs.notmuch pkgs.msmtp pkgs.jq ];
      name = "mail-send";
      script = ./mail-send.sh;
    };
    mail-reply = wrapScript {
      packages = [ much-current-query pkgs.notmuch pkgs.gnused pkgs.jq ];
      name = "mail-reply";
      script = ./mail-reply.sh;
    };
    mail-kill = wrapScript {
      name = "mail-kill";
      script = ./mail-kill.sh;
      packages = [ pkgs.notmuch ];
    };
  in pkgs.symlinkJoin {
    name = "much-scripts";
    paths = [ mail-send much-current-query mail-reply mail-kill ];
  };

  showkeys-toggle = pkgs.writers.writeDashBin "showkeys-toggle" ''
    if ${pkgs.procps}/bin/pgrep screenkey; then
      exec ${pkgs.procps}/bin/pkill screenkey
    else
      exec ${pkgs.screenkey}/bin/screenkey
    fi
  '';

  qrpaste = pkgs.writers.writeDashBin "qrpaste" ''
    file="$(${pkgs.mktemp}/bin/mktemp qrpasteXXX.png --tmpdir)"
    ${pkgs.qrencode}/bin/qrencode "$(${pkgs.xclip}/bin/xclip -selection clipboard -out)" -o "$file"
    ${pkgs.sxiv}/bin/sxiv "$file" ; rm "$file"
  '';

  interdimensional-cable =
  let nimaid-github-io = pkgs.fetchFromGitHub {
    owner = "nimaid";
    repo = "nimaid.github.io";
    rev = "9cb4ede215be6bb01bd2df1ef3e9689cc8c4eb9e";
    sha256 = "1g47cj5an7xgmhpc09m7qim5j9rspqxvnzfy90cnlvz4pg8hil96";
  };
  in pkgs.writeShellScriptBin "interdimensional-cable" ''
    export PATH=${lib.makeBinPath [ pkgs.mpv pkgs.jq pkgs.gnused ]}
    mpv --shuffle --playlist=<(jq -r '.videos[]' ${nimaid-github-io}/tv/interdimensional_database.json | sed 's#^#https://youtu.be/#')
  '';

  tag = wrapScript {
    packages = [ pkgs.vorbisTools pkgs.python3Packages.eyeD3 opustags ];
    script = "${voidrice}/.local/bin/tag";
    name = "tag";
  };

  meteo = wrapScript {
    packages = [ pkgs.jq pkgs.curl pkgs.xdotool pkgs.sxiv pkgs.gnused ];
    script = ./meteo.sh;
    name = "meteo";
  };

  kirciuoklis = wrapScript {
    packages = [ pkgs.curl pkgs.jq ];
    script = ./kirciuoklis.sh;
    name = "kirciuoklis";
  };

  booksplit = wrapScript {
    packages = [ pkgs.ffmpeg tag pkgs.glibc.bin ];
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
    packages = [ pkgs.curl pkgs.pup betacode pkgs.gnused pkgs.pandoc pkgs.man ];
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

  playlist = import ./pls.nix { inherit pkgs; };

  mpv-tv = import ./mpv-tv.nix { inherit pkgs lib; };

  favicon = wrapScript {
    packages = [ pkgs.imagemagick ];
    name = "favicon";
    script = ./favicon.sh;
  };

  closest = pkgs.writers.writeDashBin "closest" ''
    ${pkgs.writers.writeHaskellBin "closest" {
      libraries = with pkgs.haskellPackages; [ parallel optparse-applicative edit-distance ];
      ghcArgs = ["-O3" "-threaded" ];
    } (builtins.readFile ./distance.hs)}/bin/closest +RTS -N4 -RTS --dictionary ${pkgs.fetchurl {
      url = "https://gist.github.com/MarvinJWendt/2f4f4154b8ae218600eb091a5706b5f4/raw/36b70dd6be330aa61cd4d4cdfda6234dcb0b8784/wordlist-german.txt";
      sha256 = "0vr4lmlckgvj4s8sk502sknq9pf3297rvasj5sqqm05zzbdgpppj";
    }} "$@"
  '';

  horoscope = pkgs.callPackage ./horoscope {};

  genius = wrapScript {
    packages = [ pkgs.curl pkgs.gnused pkgs.pandoc ];
    name = "genius";
    script = ./genius.sh;
  };

  literature-quote = wrapScript {
    packages = [ pkgs.xsv pkgs.curl pkgs.gnused ];
    name = "literature-quote";
    script = ./literature-quote.sh;
  };

  man-pdf = wrapScript {
    packages = [ pkgs.man pkgs.ghostscript ];
    name = "man-pdf";
    script = ./man-pdf.sh;
  };

  odyssey = wrapScript {
    packages = [ pkgs.curl pkgs.xmlstarlet ];
    name = "odyssey";
    script = ./odyssey.sh;
  };

  tolino-screensaver = wrapScript {
    packages = [ pkgs.imagemagick ];
    name = "tolino-screensaver";
    script = ./tolino-screensaver.sh;
  };

  wttr = wrapScript {
    packages = [ pkgs.curl ];
    name = "wttr";
    script = ./wttr.sh;
  };

  vg = wrapScript {
    packages = [ pkgs.ripgrep pkgs.fzf pkgs.gawk ];
    name = "vg";
    script = ./vg.sh;
  };

  dmenurandr = wrapScript {
    packages = [ pkgs.dmenu pkgs.gnugrep pkgs.gnused pkgs.xorg.xrandr pkgs.gawk pkgs.libnotify pkgs.arandr ];
    name = "dmenurandr";
    script = ./dmenurandr.sh;
  };

  dmenubluetooth = wrapScript {
    packages = [ pkgs.bluez5 pkgs.dmenu pkgs.libnotify ];
    name = "dmenubluetooth";
    script = ./dmenubluetooth.sh;
  };

  fkill = wrapScript {
    packages = [ pkgs.procps pkgs.gawk pkgs.gnused pkgs.fzf pkgs.bash ];
    script = ./fkill.sh;
    name = "fkill";
  };

  nix-git = wrapScript {
    packages = [ pkgs.nix-prefetch-git pkgs.jq ];
    script = ./nix-git.sh;
    name = "nix-git";
  };

  linkhandler = wrapScript {
    packages =
      [ pkgs.utillinux pkgs.mpv pkgs.curl pkgs.gnused pkgs.sxiv pkgs.ts ];
    script = "${voidrice}/.local/bin/linkhandler";
    name = "linkhandler";
  };

  mansplain = wrapScript {
    packages = [ pkgs.man pkgs.zathura pkgs.dmenu pkgs.gnused ];
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
    packages = [ pkgs.st pkgs.fzf pkgs.dash pkgs.bash ];
  };

  swallow = wrapScript {
    script = ./swallow.sh;
    name = "swallow";
    packages = [ pkgs.xdo ];
  };

  ipa = wrapScript {
    script = ./ipa.py;
    name = "ipa";
    packages = [ pkgs.python3 ];
  };

  default-gateway = pkgs.writers.writeDashBin "default-gateway" ''
    ${pkgs.iproute}/bin/ip -json route | ${pkgs.jq}/bin/jq --raw-output '.[0].gateway'
  '';

  betacode = pkgs.writers.writePython3Bin "betacode" {
    libraries = [ betacode ];
  } ''
    import betacode.conv
    import sys

    sys.stdout.write(betacode.conv.beta_to_uni(sys.stdin.read()))
  '';

  devanagari = pkgs.callPackage ../devanagari {};

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
    packages = [ pkgs.xclip pkgs.scrot kpaste pkgs.libnotify pkgs.dmenu ];
  };

  unicodmenu = pkgs.callPackage ./unicodmenu.nix { };

  mpv-radio =
  let
    streams = import <niveum/lib/streams.nix> {
      di-fm-key = lib.strings.fileContents <secrets/di.fm/key>;
    };
    streams-tsv = pkgs.writeText "streams.tsv" (lib.concatMapStringsSep "\n" ({ desc ? "", stream, station, ... }: "${station}\t${desc}\t${stream}") streams);
  in pkgs.writers.writeDashBin "mpv-radio" ''
    exec ${pkgs.mpv}/bin/mpv --force-window=yes "$(${pkgs.dmenu}/bin/dmenu -i -l 5 < ${streams-tsv} | ${pkgs.coreutils}/bin/cut -f3)"
  '';

  rfc = wrapScript {
    script = ./rfc.sh;
    name = "rfc";
    packages = [ pkgs.curl pkgs.pup pkgs.gnused pkgs.gawk pkgs.gnugrep pkgs.fzf ];
  };

  new-mac = wrapScript {
    script = ./new-mac.sh;
    name = "new-mac";
    packages = [ pkgs.gnused pkgs.openssl pkgs.jq pkgs.iproute2 ];
  };

  bvg = pkgs.callPackage ./bvg.nix { };
  nav = pkgs.callPackage ./nav.nix { };
  k-lock = pkgs.callPackage ./k-lock.nix { };

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

    ${pkgs.imagemagick}/bin/convert -density 150 "$1" -colorspace gray -linear-stretch 3.5%x10% -blur 0x0.5 -attenuate 0.25 +noise Gaussian "scanned-$1"
  '';

  nix-index-update = pkgs.writers.writeDashBin "nix-index-update" ''
    filename="index-x86_64-$(uname | tr A-Z a-z)"
    mkdir -p ~/.cache/nix-index
    cd ~/.cache/nix-index
    # -N will only download a new version if there is an update.
    ${pkgs.wget}/bin/wget -q -N https://github.com/Mic92/nix-index-database/releases/latest/download/$filename
    ln -f $filename files
  '';
} // {
  devour = pkgs.callPackage <niveum/packages/devour.nix> { };
  depp = pkgs.callPackage <niveum/packages/depp.nix> { };
  text2pdf = pkgs.callPackage <niveum/packages/text2pdf.nix> { };
  vimv = pkgs.callPackage <niveum/packages/vimv.nix> { };
  when = pkgs.callPackage <niveum/packages/when.nix> { };
  mahlzeit = pkgs.haskellPackages.callPackage <niveum/packages/mahlzeit.nix> { };
  inherit opustags;
}
