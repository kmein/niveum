{ stdenv, fetchFromGitHub }:
stdenv.mkDerivation rec {
  name = "git-quick-stats";
  version = "2.0.8";
  src = fetchFromGitHub {
    repo = name;
    owner = "arzzen";
    rev = "c11bce17bdb1c7f1272c556605b48770646bf807";
    sha256 = "1px1sk7b6mjnbclsr1jn33m9k4wd8wqyw4d6w1rgj0ii29lhzmqi";
  };
  installPhase = ''
    PREFIX=$out make install
  '';
}
