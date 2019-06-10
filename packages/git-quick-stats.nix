{ stdenv, fetchFromGitHub }:
stdenv.mkDerivation rec {
  name = "${pname}-${version}";
  pname = "git-quick-stats";
  version = "2.0.8";
  src = fetchFromGitHub {
    repo = "git-quick-stats";
    owner = "arzzen";
    rev = "${version}";
    sha256 = "1px1sk7b6mjnbclsr1jn33m9k4wd8wqyw4d6w1rgj0ii29lhzmqi";
  };
  installPhase = ''
    PREFIX=$out make install
  '';
  meta = with stdenv.lib; {
    homepage = "https://github.com/arzzen/git-quick-stats";
    description = "Git quick statistics is a simple and efficient way to access various statistics in git repository.";
    platforms = platforms.all;
    maintainers = [ maintainers.kmein ];
    license = licenses.mit;
  };
}
