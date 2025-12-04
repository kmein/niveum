{
  vimUtils,
  fetchFromGitHub,
}:
vimUtils.buildVimPlugin {
  pname = "vim-mail";
  version = "acdbb5bd";
  src = fetchFromGitHub {
    owner = "dbeniamine";
    repo = "vim-mail";
    rev = "acdbb5bdd2bc6fe5dc46e3dc7ba5e1dcb81630c2";
    sha256 = "1q4ly3spnh8hx6q8yihbi6rcjd1hd2r5fllm5lwnq9dmlj6l1l10";
  };
}
