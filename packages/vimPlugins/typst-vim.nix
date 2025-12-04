{
  vimUtils,
  fetchFromGitHub,
}:
vimUtils.buildVimPlugin {
  pname = "typst.vim";
  version = "2882f21";
  src = fetchFromGitHub {
    owner = "kaarmu";
    repo = "typst.vim";
    rev = "2882f211f1498c790bb857f8a912c8e86526a362";
    sha256 = "0xr8k17ggqfdksf3kybimfl5djjz3h19k4479la06i5lnwvlhkh2";
  };
}
