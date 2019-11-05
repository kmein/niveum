{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    texlive.combined.scheme-full
    (aspellWithDicts (dict: [dict.de dict.en dict.la dict.en-computers dict.ru]))
    unstable.haskellPackages.pandoc-citeproc
    libreoffice
    unstable.pandoc
    proselint
    unstable.asciidoctor
    wordnet
  ];
}
