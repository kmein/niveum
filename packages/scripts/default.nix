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
    rev = "dff66cd1efb36afd54dd6dcf2fdaa9475d5646c1";
    sha256 = "19f33ins2kzgiw72d62j8zz9ai3j8m4qqfqmagxkg9yhxqkdqry7";
  };
in {
  instaget = wrapScript {
    packages = [ pkgs.jq pkgs.curl pkgs.gnugrep ];
    script = ./instaget.sh;
    name = "instaget";
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

  betacode = pkgs.writers.writePython3Bin "betacode" {
    libraries = [ pkgs.nur.repos.kmein.python3Packages.betacode ];
  } ''
    import betacode.conv
    import sys

    sys.stdout.write(betacode.conv.beta_to_uni(sys.stdin.read()))
  '';

  scrot-dmenu = wrapScript {
    script = ./scrot-dmenu.sh;
    name = "dmenu-scrot";
    packages = [ pkgs.xclip pkgs.scrot kpaste pkgs.libnotify pkgs.dmenu ];
  };

  bvg = pkgs.callPackage ./bvg.nix { };
  nav = pkgs.callPackage ./nav.nix { };
}
