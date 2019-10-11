{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # (texlive.combine {
    #   inherit (pkgs.texlive) scheme-full texdoc latex2e-help-texinfo;
    #   pkgFilter = pkg: pkg.tlType == "run" || pkg.tlType == "bin" || pkg.pname == "latex2e-help-texinfo";
    # })
    # haskellPackages.patat
    (aspellWithDicts (dict: [dict.de dict.en dict.la dict.en-computers dict.ru]))
    haskellPackages.pandoc-citeproc
    libreoffice
    pandoc
    proselint
    unstable.asciidoctor
    wordnet
  ];
}
