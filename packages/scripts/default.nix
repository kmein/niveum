{ pkgs, lib, ... }:
let
  kpaste = pkgs.callPackage <stockholm/krebs/5pkgs/simple/kpaste> { };
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
    packages = [ pkgs.vorbisTools pkgs.python3Packages.eyeD3 pkgs.nur.repos.kmein.opustags ];
    script = "${voidrice}/.local/bin/tag";
    name = "tag";
  };

  meteo = wrapScript {
    packages = [ pkgs.jq pkgs.curl pkgs.xdotool pkgs.sxiv pkgs.gnused ];
    script = ./meteo.sh;
    name = "meteo";
  };

  booksplit = wrapScript {
    packages = [ pkgs.ffmpeg tag ];
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

  boetlingk = wrapScript {
    name = "boet";
    script = ./boetlingk.sh;
    packages = [ pkgs.curl pkgs.gnused pkgs.pandoc pkgs.man ];
  };

  playlist = wrapScript {
    name = "pls";
    script = ./playlist.sh;
    packages = [ pkgs.curl pkgs.jq ];
  };

  favicon = wrapScript {
    packages = [ pkgs.imagemagick ];
    name = "favicon";
    script = ./favicon.sh;
  };

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
    libraries = [ pkgs.nur.repos.kmein.python3Packages.betacode ];
  } ''
    import betacode.conv
    import sys

    sys.stdout.write(betacode.conv.beta_to_uni(sys.stdin.read()))
  '';

  devanagari = pkgs.writers.writePython3Bin "devanagari" {
    libraries = [ pkgs.nur.repos.kmein.python3Packages.indic-transliteration ];
  } ''
    from indic_transliteration import sanscript
    from indic_transliteration.sanscript import transliterate
    import sys

    # Harvard Kyoto -> Devanagari
    sys.stdout.write(transliterate(
      sys.stdin.read(),
      sanscript.HK,
      sanscript.DEVANAGARI
    ))
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
    exec ${pkgs.mpv}/bin/mpv "$(${pkgs.fzf}/bin/fzf < ${streams-tsv} | ${pkgs.coreutils}/bin/cut -f3)"
  '';

  rfc = wrapScript {
    script = ./rfc.sh;
    name = "rfc";
    packages = [ pkgs.curl pkgs.pup pkgs.gnused pkgs.gawk pkgs.gnugrep pkgs.fzf ];
  };

  bvg = pkgs.callPackage ./bvg.nix { };
  nav = pkgs.callPackage ./nav.nix { };
  k-lock = pkgs.callPackage ./k-lock.nix { };
}
