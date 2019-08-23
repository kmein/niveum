{ vimUtils, fetchFromGitHub }:
vimUtils.buildVimPluginFrom2Nix {
  name = "todo.txt-vim";
  src = fetchFromGitHub {
    owner = "freitass";
    repo = "todo.txt-vim";
    rev = "6845221d45bd62e604c2024bc511a56e79d1118b";
    sha256 = "08m9q5f2pz6gjp0vkmm7glfsrbnldxi1j59dm5d7any6y96xxd6v";
  };
}
