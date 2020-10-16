{ pkgs, lib, config, ... }:
let
  zoteroStyle = { name, sha256 }: {
    name = "${name}.csl";
    path = pkgs.fetchurl {
      url = "https://www.zotero.org/styles/${name}";
      inherit sha256;
    };
  };

  cslDirectory = pkgs.linkFarm "citation-styles" [
    (zoteroStyle {
      name = "chicago-author-date-de";
      sha256 = "0fz0xn46rkciblr34a7x2v60j0lbq9l3fmzi43iphph27m0czn6s";
    })
    (zoteroStyle {
      name = "din-1505-2";
      sha256 = if scardanelli then
        "1pvy1b7qm13mnph7z365rrz1j082bl2y8ih73rhzd0zd6dz1jyjq"
      else
        "150kbnxl1r4g1s40khdavv5s6ah10ws135r9k883f6srk78sz6zi";
    })
    (zoteroStyle {
      name = "apa";
      sha256 = if scardanelli then
        "0g8vhp7gnd315h5b60r3zqp49kaq3fkxqnz2v7j2a0zp6s3cisdk"
      else
        "1rg41mblmqifba1azb6481dwxhsbl606kf6ysqkqd786f9l9dcf8";
    })
  ];

  makeStardictDataDir = dicts:
    pkgs.linkFarm "dictionaries" (map ({ name, path }: {
      name = "dic/${name}";
      inherit path;
    }) dicts);
  scardanelli = config.networking.hostName == "scardanelli";
in {
  environment.variables.STARDICT_DATA_DIR = toString (makeStardictDataDir [
    {
      name = "gr-de";
      path = pkgs.fetchurl {
        url = "http://tovotu.de/data/stardict/pape_gr-de.zip";
        sha256 = "1d705y47b40vp0mg79vbwasw4y0i8fmnlwvf4x4ri0dkfqng9sky";
      };
    }
    {
      name = "la-de";
      path = pkgs.fetchurl {
        url = "http://tovotu.de/data/stardict/georges_lat-de.zip";
        sha256 = "12n26nzwg28wn4zwv45mv0wkgy1jh1d8p0k6haamz9601cqq7hkj";
      };
    }
    {
      name = "de-la";
      path = pkgs.fetchurl {
        url = "http://tovotu.de/data/stardict/georges_de-lat.zip";
        sha256 = "0inm6xn1lcnb851cj329n0v2vbfc1z1bxwhgsd8fnm0zxy3f3ifq";
      };
    }
  ]);

  home-manager.users.me.home.file = {
    ".csl".source = cslDirectory;
    ".local/share/pandoc/csl".source = cslDirectory; # as of pandoc 2.11, it includes citeproc
  };

  environment.systemPackages = with pkgs; [
    texlive.combined.scheme-full
    (aspellWithDicts (dict: [ dict.de dict.en dict.en-computers ]))
    haskellPackages.pandoc-citeproc
    # nur.repos.kmein.text2pdf
    libreoffice
    unstable.pandoc
    # proselint
    asciidoctor
    wordnet
    sdcv # stardict cli
  ];
}
