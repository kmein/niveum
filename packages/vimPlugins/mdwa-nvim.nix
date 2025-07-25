{
  vimUtils,
  fetchFromGitHub,
  lib,
}: (vimUtils.buildVimPluginFrom2Nix {
  pname = "mdwa.nvim";
  version = "9f37270";
  src = fetchFromGitHub {
    owner = "tihawk";
    repo = "mdwa.nvim";
    rev = "9f3727037e0d85fd0930334b91b9687a5a880192";
    hash = "sha256-h2jy2E+pN2Ma/5n9Eq2oXr9xHma2OxxVvx9EJ+bIYxA=";
  };
})
