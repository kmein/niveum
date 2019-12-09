{ pkgs, lib, ... }:
let
  zoteroStyle = { name, sha256 }: {
    name = "${name}.csl";
    path = pkgs.fetchurl {
      url = "https://www.zotero.org/styles/${name}";
      inherit sha256;
    };
  };
in
{
  environment.variables.CITATIONSTYLES = toString (pkgs.linkFarm "citation-styles" [
    (zoteroStyle {
      name = "chicago-author-date-de";
      sha256 = "0fz0xn46rkciblr34a7x2v60j0lbq9l3fmzi43iphph27m0czn6s";
    })
    (zoteroStyle {
      name = "din-1505-2";
      sha256 = "150kbnxl1r4g1s40khdavv5s6ah10ws135r9k883f6srk78sz6zi";
    })
    (zoteroStyle {
      name = "apa";
      sha256 = "1rg41mblmqifba1azb6481dwxhsbl606kf6ysqkqd786f9l9dcf8";
    })
  ]);

  environment.systemPackages = with pkgs; [
    texlive.combined.scheme-full
    (aspellWithDicts (dict: [dict.de dict.en dict.en-computers]))
    unstable.haskellPackages.pandoc-citeproc
    text2pdf
    libreoffice
    unstable.pandoc
    # proselint
    unstable.asciidoctor
    wordnet
  ];
}
