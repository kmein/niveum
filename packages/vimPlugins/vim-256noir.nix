{ vimUtils, fetchFromGitHub }:
vimUtils.buildVimPluginFrom2Nix {
  name = "vim-256noir";
  src = fetchFromGitHub {
    owner = "andreasvc";
    repo = "vim-256noir";
    rev = "e8668a18a4a90272c1cae87e655f8bddc5ac3665";
    sha256 = "1kpn379f5dgbsgb73g6d1nlmz9vz0j3ihi500mcdx4yg56fvkr0x";
  };
}
