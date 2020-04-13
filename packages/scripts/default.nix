{ pkgs, lib, ... }:
let
  inherit (lib.strings) makeBinPath;
  makeScript = { binPath ? [], name, src }: pkgs.writeScriptBin name (builtins.readFile src);
in
{
  # https://github.com/LukeSmithxyz/voidrice/blob/9fe6802122f6e0392c7fe20eefd30437771d7f8e/.local/bin/dmenuunicode
  emoji-menu =
  let emoji-file = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/LukeSmithxyz/voidrice/master/.local/share/larbs/emoji";
    sha256 = "09m2rgb9d5jpiy8q4jz3dw36gkpb4ng2pl7xi7ppsrzzzdvq85qk";
  };
  in with pkgs; writers.writeDashBin "emoji-menu" ''
    PATH=${makeBinPath [ coreutils dmenu gnused libnotify xclip xdotool ]}
    chosen=$(cut -d ';' -f1 ${emoji-file} | dmenu -i -l 10 | sed "s/ .*//")

    [ "$chosen" != "" ] || exit

    echo "$chosen" | tr -d '\n' | xclip -selection clipboard

    if [ -n "$1" ]; then
      xdotool key Shift+Insert
    else
      notify-send "'$chosen' copied to clipboard." &
    fi
  '';

  instaget = makeScript {
    binPath = [ pkgs.jq pkgs.curl pkgs.gnugrep ];
    src = ./instaget.sh;
    name = "instaget";
  };

  n = makeScript {
    src = ./n.sh;
    name = "n";
  };

  dirmir = makeScript {
    name = "dirmir";
    src = ./dirmir.sh;
  };

  favicon = makeScript {
    binPath = [ pkgs.imagemagick ];
    name = "favicon";
    src = ./favicon.sh;
  };

  genius = makeScript {
    binPath = [ pkgs.curl pkgs.gnused pkgs.pandoc ];
    name = "genius";
    src = ./genius.sh;
  };

  literature-quote = makeScript {
    binPath = [ pkgs.xsv pkgs.curl pkgs.gnused ];
    name = "literature-quote";
    src = ./literature-quote.sh;
  };

  man-pdf = makeScript {
    binPath = [ pkgs.man pkgs.ghostscript ];
    name = "man-pdf";
    src = ./man-pdf.sh;
  };

  odyssey = makeScript {
    binPath = [ pkgs.curl pkgs.xmlstarlet ];
    name = "odyssey";
    src = ./odyssey.sh;
  };

  tolino-screensaver = makeScript {
    binPath = [ pkgs.imagemagick ];
    name = "tolino-screensaver";
    src = ./tolino-screensaver.sh;
  };

  wttr = makeScript {
    binPath = [ pkgs.curl ];
    name = "wttr";
    src = ./wttr.sh;
  };

  vf = makeScript {
    binPath = [ pkgs.fd pkgs.fzf ];
    name = "vf";
    src = ./vf.sh;
  };

  vg = makeScript {
    binPath = [ pkgs.ripgrep pkgs.fzf pkgs.gawk ];
    name = "vg";
    src = ./vg.sh;
  };

  fkill = makeScript {
    binPath = [ pkgs.procps pkgs.gawk pkgs.gnused pkgs.fzf ];
    src = ./fkill.sh;
    name = "fkill";
  };

  nix-git = makeScript {
    binPath = [ pkgs.nix-prefetch-git pkgs.jq ];
    src = ./nix-git.sh;
    name = "nix-git";
  };

  notetags = makeScript {
    src = ./notetags.sh;
    name = "notetags";
  };

  fzfmenu = makeScript {
    src = ./fzfmenu.sh;
    name = "fzfmenu";
    binPath = [ pkgs.st pkgs.fzf ];
  };

  bvg = pkgs.callPackage ./bvg.nix {};
  nav = pkgs.callPackage ./nav.nix {};
}
