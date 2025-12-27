{
  vimUtils,
  fetchFromGitHub,
}:
(vimUtils.buildVimPlugin {
  pname = "cheat.sh-vim";
  version = "826219d1";
  src = fetchFromGitHub {
    owner = "dbeniamine";
    repo = "cheat.sh-vim";
    rev = "826219d16af492413e427fd666f3136f8e3fb0b0";
    sha256 = "09bvyb1xk2lrwacw3mvyd2f8a9g4hvs10q3s1k39b20p1x3bbzfn";
  };
})
