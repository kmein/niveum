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
      sha256 = "1pvy1b7qm13mnph7z365rrz1j082bl2y8ih73rhzd0zd6dz1jyjq";
    })
    (zoteroStyle {
      name = "apa";
      sha256 = "0g8vhp7gnd315h5b60r3zqp49kaq3fkxqnz2v7j2a0zp6s3cisdk";
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
