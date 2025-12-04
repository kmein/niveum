{
  vimUtils,
  fetchFromGitHub,
  lib,
}: (vimUtils.buildVimPlugin {
  pname = "vim-ernest";
  version = "4b99bc3";
  src = fetchFromGitHub {
    owner = "lgalke";
    repo = "vim-ernest";
    rev = "4b99bc3fe3deb7bb958ad2f64cad93569eeb50d7";
    hash = "sha256-AUuRnnZU39XUerBxNelEqVyDAalRm3VGNUQb15fjXjM=";
  };
})
