{
  pkgs,
  niveumPackages,
  ...
}: let
  zip-font = name: arguments: let
    directory = pkgs.fetchzip arguments;
  in
    pkgs.runCommand name {} ''
      mkdir -p $out/share/fonts/{truetype,opentype,woff}
      ${pkgs.findutils}/bin/find ${directory} -name '*.ttf' -exec install '{}' $out/share/fonts/truetype \;
      ${pkgs.findutils}/bin/find ${directory} -name '*.otf' -exec install '{}' $out/share/fonts/opentype \;
      ${pkgs.findutils}/bin/find ${directory} -name '*.woff' -exec install '{}' $out/share/fonts/woff \;
    '';
  simple-ttf = name: arguments: let
    file = pkgs.fetchurl arguments;
  in
    pkgs.runCommand name {} ''
      mkdir -p $out/share/fonts/truetype
      install ${file} $out/share/fonts/truetype
    '';

  egyptianHiero = zip-font "EgyptianHiero" {
    url = "https://github.com/MKilani/Djehuty/archive/master.zip";
    sha256 = "sha256-KbY4vedm757NWfDlgmNhslbZd+2Vs+o5PjtMMGDt61Y=";
  };
  brill = zip-font "Brill" {
    url = "https://brill.com/fileasset/The_Brill_Typeface_Package_v_4_0.zip";
    stripRoot = false;
    hash = "sha256-ugmEIkeBzD/4C9wkVfbctEtnzI8Kw+YD6KGcbk4BAf4=";
  };
  antinoou = zip-font "Antinoou" {
    url = "https://www.evertype.com/fonts/coptic/AntinoouFont.zip";
    sha256 = "0jwihj08n4yrshcx07dnaml2x9yws6dgyjkvg19jqbz17drbp3sw";
    stripRoot = false;
  };
  newGardiner = zip-font "NewGardiner" {
    url = "https://mjn.host.cs.st-andrews.ac.uk/egyptian/fonts/NewGardiner.zip";
    hash = "sha256-nP0y4ILt+0mlkDRdCNSeO2Gequ8wyix/qQdmujTNw3Y=";
    stripRoot = false;
  };
  newAthenaUnicode = zip-font "NewAthenaUnicode" {
    url = "https://classicalstudies.org/sites/default/files/userfiles/files/NAU5_005.zip";
    sha256 = "1g7qk9gl4nq2dz41bvck1nzilhin44j8691cxax3dlp77bbn9bxr";
  };
  jsesh = simple-ttf "JSesh" {
    url = "http://files.qenherkhopeshef.org/jsesh/JSeshFont.ttf";
    sha256 = "1203jrk2xzvgckcc5hx88kja1i3h8gm1wiyla5j6gspc0hbv56ry";
  };
  egyptianTextBeta = simple-ttf "EgyptianText-1.0beta" {
    url = "http://c.krebsco.de/EgyptianText-v1.0-beta.ttf";
    sha256 = "0cfjbk7xxnxhlp6v922psm5j1xzrv6wfk226ji2wz2yfrnkbcbsv";
  };
  coranica = simple-ttf "Coranica" {
    url = "https://corpuscoranicum.de/fonts/coranica_1164.ttf";
    sha256 = "0igi8q8b2p38x9jq8c98afsl7bf8rj32zj2052yyjgj9r88y4yi5";
  };
  koineGreek = simple-ttf "KoineGreek.ttf" {
    url = "https://github.com/Center-for-New-Testament-Restoration/font/raw/af83eed50105344edaa5e5eddaf87696e271468c/KoineGreek.ttf";
    hash = "sha256-YtC+nj7+Jl8k00rqAAqySYc8iTAOL7PixXc+LfSmnS0=";
  };
  egyptianText = simple-ttf "EgyptianText" {
    url = "https://github.com/microsoft/font-tools/raw/1092cb23520967830001a0807eb21d6a44dda522/EgyptianOpenType/font/eot.ttf";
    sha256 = "1n294vhcx90270pnsw1dbk6izd61fjvbnjrh4hcf98ff3s540x0c";
  };
in {
  fonts = {
    enableDefaultPackages = true;
    fontDir.enable = true;
    packages = with pkgs; [
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
      coranica
      corefonts
      crimson
      eb-garamond
      ipaexfont
      jsesh
      egyptianHiero
      egyptianText
      egyptianTextBeta
      font-awesome_6
      etBook
      newGardiner
      junicode
      koineGreek
      brill
      ezra-sil
      fira
      font-awesome
      galatia-sil
      gentium
      # niveumPackages.gfs-fonts
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
      niveumPackages.tocharian-font
      vistafonts
      vollkorn
      zilla-slab
    ]; # google-fonts league-of-moveable-type
    fontconfig.defaultFonts = rec {
      monospace = ["Noto Sans Mono"] ++ emoji;
      serif = ["Noto Serif" "Noto Naskh Arabic" "Noto Serif Devanagari"];
      sansSerif = ["Noto Sans Display" "Noto Naskh Arabic" "Noto Sans Hebrew" "Noto Sans Devanagari" "Noto Sans CJK JP" "Noto Sans Coptic"];
      emoji = ["Noto Color Emoji"];
    };
    # xelatex fails with woff files
    # ref https://tex.stackexchange.com/questions/392144/xelatex-and-fontspec-crash-trying-to-find-woff-file-for-some-fonts-but-not-other
    fontconfig.localConf = ''
      <fontconfig>
      <!-- Reject WOFF fonts We don't register WOFF(2) fonts with fontconfig because of the W3C spec -->
       <selectfont>
        <rejectfont>
         <glob>*.woff*</glob>
        </rejectfont>
       </selectfont>
      </fontconfig>
    '';
  };
}
