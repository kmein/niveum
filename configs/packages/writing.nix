{ pkgs, ... }:
{
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
