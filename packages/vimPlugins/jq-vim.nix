{ vimUtils, fetchFromGitHub }:
vimUtils.buildVimPluginFrom2Nix {
  name = "jq.vim";
  src = fetchFromGitHub {
    owner = "vito-c";
    repo = "jq.vim";
    rev = "5baf8ed192cf267d30b84e3243d9aab3d4912e60";
    sha256 = "1ykaxlli7b9wmhr8lpdalqxh7l4940jwhwm9pwlraga425h4r6z4";
  };
}
