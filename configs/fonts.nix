{pkgs, ...}: let
  zip-ttf = name: arguments: let
    directory = pkgs.fetchzip arguments;
  in
    pkgs.runCommand name {} ''
      mkdir -p $out/share/fonts/truetype
      ${pkgs.findutils}/bin/find ${directory} -name '*.ttf' -exec install '{}' $out/share/fonts/truetype \;
    '';
  zip-otf = name: arguments: let
    directory = pkgs.fetchzip arguments;
  in
    pkgs.runCommand name {} ''
      mkdir -p $out/share/fonts/opentype
      ${pkgs.findutils}/bin/find ${directory} -name '*.otf' -exec install '{}' $out/share/fonts/opentype \;
    '';
  simple-ttf = name: arguments: let
    file = pkgs.fetchurl arguments;
  in
    pkgs.runCommand name {} ''
      mkdir -p $out/share/fonts/truetype
      install ${file} $out/share/fonts/truetype
    '';

  egyptianHiero = zip-ttf "EgyptianHiero" {
    url = "https://github.com/MKilani/Djehuty/archive/master.zip";
    sha256 = "0xaq16ysvxrkcn3264wkmm2ln0hpijpk4iq1n5i7d9gqhjhsav1x";
  };
  antinoou = zip-ttf "Antinoou" {
    url = "https://www.evertype.com/fonts/coptic/AntinoouFont.zip";
    sha256 = "0jwihj08n4yrshcx07dnaml2x9yws6dgyjkvg19jqbz17drbp3sw";
    stripRoot = false;
  };
  newGardiner = zip-ttf "NewGardiner" {
    url = "https://mjn.host.cs.st-andrews.ac.uk/egyptian/fonts/NewGardiner.zip";
    sha256 = "1jd0qa6shh9pqqyig2w43m9l9rv1i50l73jzkhb6g6mqxbhb1mip";
    stripRoot = false;
  };
  junicode2 = zip-otf "JunicodeTwo" {
    url = "https://github.com/psb1558/Junicode-font/archive/48bf476db278c844c67542b04d1e0e4c71f139d2.zip";
    sha256 = "1ryicc155vkvgv3315ddliigwa01afwyb4c4f6pnqcns03af001i";
  };
  newAthenaUnicode = zip-ttf "NewAthenaUnicode" {
    url = "https://classicalstudies.org/sites/default/files/userfiles/files/NAU5_005.zip";
    sha256 = "1g7qk9gl4nq2dz41bvck1nzilhin44j8691cxax3dlp77bbn9bxr";
  };
  jsesh = simple-ttf "JSesh" {
    url = "http://files.qenherkhopeshef.org/jsesh/JSeshFont.ttf";
    sha256 = "1203jrk2xzvgckcc5hx88kja1i3h8gm1wiyla5j6gspc0hbv56ry";
  };
  egyptianText = simple-ttf "EgyptianText-1.0beta" {
    url = "http://c.krebsco.de/EgyptianText-v1.0-beta.ttf";
    sha256 = "0cfjbk7xxnxhlp6v922psm5j1xzrv6wfk226ji2wz2yfrnkbcbsv";
  };
in {
  fonts = {
    enableDefaultFonts = true;
    fontDir.enable = true;
    fonts = with pkgs; [
      alegreya
      alegreya-sans
      amiri
      annapurna-sil
      antinoou
      cantarell-fonts
      cardo
      charis-sil
      doulos-sil
      newAthenaUnicode
      corefonts
      crimson
      eb-garamond
      jsesh
      egyptianHiero
      egyptianText
      font-awesome_6
      etBook
      newGardiner
      junicode2
      ezra-sil
      fira
      font-awesome
      galatia-sil
      gentium
      gfs-fonts
      gyre-fonts
      ibm-plex
      jetbrains-mono
      libertinus
      libre-bodoni
      lmodern
      merriweather
      ocr-a
      roboto
      roboto-mono
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      roboto-slab
      scheherazade-new
      source-code-pro
      source-sans-pro
      source-serif-pro
      theano
      tocharian-font
      vistafonts
      vollkorn
      zilla-slab
    ]; # google-fonts league-of-moveable-type
    fontconfig.defaultFonts = rec {
      monospace = ["Noto Sans Mono"] ++ emoji;
      serif = ["Noto Serif" "Noto Naskh Arabic" "Noto Serif Devanagari"];
      sansSerif = ["Noto Sans Display" "Noto Kufi Arabic" "Noto Sans Devanagari" "Noto Sans CJK JP"];
      emoji = ["Noto Color Emoji"];
    };
  };
}
