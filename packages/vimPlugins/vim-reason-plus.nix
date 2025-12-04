{
  vimUtils,
  fetchFromGitHub,
}: (vimUtils.buildVimPlugin {
  pname = "vim-reason-plus";
  version = "c11a2940";
  src = fetchFromGitHub {
    owner = "reasonml-editor";
    repo = "vim-reason-plus";
    rev = "c11a2940f8f3e3915e472c366fe460b0fe008bac";
    sha256 = "1vx7cwxzj6f12qcwcwa040adqk9cyzjd9f3ix26hnw2dw6r9cdr4";
  };
})
