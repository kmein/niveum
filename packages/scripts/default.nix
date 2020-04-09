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

  bvg = pkgs.callPackage ./bvg.nix {};
  daybook = pkgs.callPackage ./daybook.nix {};
  depp = pkgs.callPackage ./depp.nix {};
  nav = pkgs.callPackage ./nav.nix {};
  slide =
    let slide-package = pkgs.fetchFromGitHub {
      owner = "kmein";
      repo = "slide";
      rev = "0470583d22212745eab4f46076267addf4d2346c";
      sha256 = "0skcp3va9v4hmxy5ramghpz53gnyxv10wsacgmc2jr0v1wrqlzbh";
    };
    in pkgs.callPackage slide-package {};
}
