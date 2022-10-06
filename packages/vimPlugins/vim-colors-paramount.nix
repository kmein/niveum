{
  vimUtils,
  fetchFromGitHub,
}:
vimUtils.buildVimPluginFrom2Nix rec {
  pname = "vim-colors-paramount";
  version = "a5601d36";
  src = fetchFromGitHub {
    owner = "owickstrom";
    repo = "vim-colors-paramount";
    rev = "a5601d36fb6932e8d1a6f8b37b179a99b1456798";
    sha256 = "0rjn9vjb0xrxbiqyfclda2ridcbl3nfn4svs32mvmv8als6crncg";
  };
}
