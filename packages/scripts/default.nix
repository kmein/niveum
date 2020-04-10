{ pkgs, ... }:
let
  makeScript = { propagatedBuildInputs ? [], name, src }: pkgs.stdenv.mkDerivation {
    inherit name src propagatedBuildInputs;
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      install $src $out/bin/${name}
    '';
  };
in
{
  instaget = makeScript {
    propagatedBuildInputs = [ pkgs.jq pkgs.curl pkgs.gnugrep ];
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
    propagatedBuildInputs = [ pkgs.imagemagick ];
    name = "favicon";
    src = ./favicon.sh;
  };

  genius = makeScript {
    propagatedBuildInputs = [ pkgs.curl pkgs.gnused pkgs.pandoc ];
    name = "genius";
    src = ./genius.sh;
  };

  literature-quote = makeScript {
    propagatedBuildInputs = [ pkgs.xsv pkgs.curl pkgs.gnused ];
    name = "literature-quote";
    src = ./literature-quote.sh;
  };

  man-pdf = makeScript {
    propagatedBuildInputs = [ pkgs.man pkgs.ghostscript ];
    name = "man-pdf";
    src = ./man-pdf.sh;
  };

  odyssey = makeScript {
    propagatedBuildInputs = [ pkgs.curl pkgs.xmlstarlet ];
    name = "odyssey";
    src = ./odyssey.sh;
  };

  tolino-screensaver = makeScript {
    propagatedBuildInputs = [ pkgs.imagemagick ];
    name = "tolino-screensaver";
    src = ./tolino-screensaver.sh;
  };

  wttr = makeScript {
    propagatedBuildInputs = [ pkgs.curl ];
    name = "wttr";
    src = ./wttr.sh;
  };

  vf = makeScript {
    propagatedBuildInputs = [ pkgs.fd pkgs.fzf ];
    name = "vf";
    src = ./vf.sh;
  };

  vg = makeScript {
    propagatedBuildInputs = [ pkgs.ripgrep pkgs.fzf pkgs.gawk ];
    name = "vg";
    src = ./vg.sh;
  };

  fkill = makeScript {
    propagatedBuildInputs = [ pkgs.procps pkgs.gawk pkgs.gnused pkgs.fzf ];
    src = ./fkill.sh;
    name = "fkill";
  };

  nix-git = makeScript {
    propagatedBuildInputs = [ pkgs.nix-prefetch-git pkgs.jq ];
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
    propagatedBuildInputs = [ pkgs.alacritty pkgs.fzf ];
  };

  bvg = pkgs.callPackage ./bvg.nix {};
  nav = pkgs.callPackage ./nav.nix {};
}
