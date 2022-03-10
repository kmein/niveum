{
  vimUtils,
  fetchFromGitHub,
}:
vimUtils.buildVimPluginFrom2Nix {
  name = "vim-fsharp";
  src = fetchFromGitHub {
    owner = "fsharp";
    repo = "vim-fsharp";
    rev = "627db7d701747e8fd7924b6505c61e16a369fb72";
    sha256 = "00hhgn2p54faysx1ddccyhl9jnvddgxsczhv0np3mgzza6ls4838";
  };
}
