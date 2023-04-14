{
  stdenv,
  fetchFromGitHub,
  itl,
  lib,
  # autoreconfHook,
  automake,
}:
stdenv.mkDerivation rec {
  pname = "itools";
  version = "1.0";
  src = fetchFromGitHub {
    owner = "arabeyes-org";
    repo = "itools";
    rev = version;
    sha256 = "sha256-g9bsjupC4Sb5ywAgUNbjYLbHZ/i994lbNSnX2JyaP3g=";
  };
  # nativeBuildInputs = [autoreconfHook];
  buildPhase = ''
    touch ChangeLog
    ./configure
    make
  '';
  buildInputs = [itl];
  meta = {
    homepage = "https://www.arabeyes.org/ITL";
    description = "The itools package is a set of user friendly applications utilizing Arabeyes' ITL library. ";
    license = lib.licenses.gpl2;
    platforms = lib.platforms.all;
    maintainer = [lib.maintainers.kmein];
  };
}
