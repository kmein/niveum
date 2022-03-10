{
  vimUtils,
  fetchFromGitHub,
}: (vimUtils.buildVimPluginFrom2Nix {
  name = "icalendar.vim";
  src = fetchFromGitHub {
    owner = "vim-scripts";
    repo = "icalendar.vim";
    rev = "542fff45385b1b5ad9781b0ad4878ba3b7ee9d5f";
    sha256 = "0sl8rfk004cagi12ghlcqz0wci1xf5raglm50pkan79jk7srckhq";
  };
})
