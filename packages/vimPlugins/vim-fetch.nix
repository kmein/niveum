{
  vimUtils,
  fetchFromGitHub,
}: (vimUtils.buildVimPlugin {
  pname = "vim-fetch";
  version = "76c08586";
  src = fetchFromGitHub {
    owner = "wsdjeg";
    repo = "vim-fetch";
    rev = "76c08586e15e42055c9c21321d9fca0677442ecc";
    sha256 = "0avcqjcqvxgj00r477ps54rjrwvmk5ygqm3qrzghbj9m1gpyp2kz";
  };
})
