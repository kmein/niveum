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

  tag = wrapScript {
    packages = [ pkgs.vorbisTools pkgs.python3Packages.eyeD3 pkgs.nur.repos.kmein.opustags ];
    script = "${voidrice}/.local/bin/tag";
    name = "tag";
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

  bvg = pkgs.callPackage ./bvg.nix { };
  nav = pkgs.callPackage ./nav.nix { };
}
