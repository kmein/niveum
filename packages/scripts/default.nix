{ pkgs, lib, ... }:
let
  wrapScript = { packages ? [], name, script }: pkgs.writers.writeDashBin name ''
    PATH=${lib.makeBinPath (packages ++ [pkgs.coreutils pkgs.findutils])}
    ${script} "$@"
  '';
in
{
  # https://github.com/LukeSmithxyz/voidrice/blob/9fe6802122f6e0392c7fe20eefd30437771d7f8e/.local/bin/dmenuunicode
  emoji-menu =
  let emoji-file = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/LukeSmithxyz/voidrice/master/.local/share/larbs/emoji";
    sha256 = "09m2rgb9d5jpiy8q4jz3dw36gkpb4ng2pl7xi7ppsrzzzdvq85qk";
  };
  in with pkgs; writers.writeDashBin "emoji-menu" ''
    PATH=${lib.makeBinPath [ coreutils dmenu gnused libnotify xclip xdotool ]}
    chosen=$(cut -d ';' -f1 ${emoji-file} | dmenu -i -l 10 | sed "s/ .*//")

    [ "$chosen" != "" ] || exit

    echo "$chosen" | tr -d '\n' | xclip -selection clipboard

    if [ -n "$1" ]; then
      xdotool key Shift+Insert
    else
      notify-send "'$chosen' copied to clipboard." &
    fi
  '';

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

  notetags = wrapScript {
    script = ./notetags.sh;
    name = "notetags";
  };

  fzfmenu = wrapScript {
    script = ./fzfmenu.sh;
    name = "fzfmenu";
    packages = [ pkgs.st pkgs.fzf pkgs.dash pkgs.bash ];
  };

  bvg = pkgs.callPackage ./bvg.nix {};
  nav = pkgs.callPackage ./nav.nix {};
}
