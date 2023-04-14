{
  stdenv,
  fetchFromGitHub,
  cmake,
  lib,
}:
stdenv.mkDerivation rec {
  pname = "itl";
  version = "0.8.0";
  src = fetchFromGitHub {
    owner = "arabeyes-org";
    repo = "ITL";
    rev = "v${version}";
    sha256 = "sha256-GTicTbZmFbPhzInFob3cfvtTxOpUZuqsQz1w9CoWu9w=";
  };
  nativeBuildInputs = [cmake];
  cmakeFlags = [
    "-DCMAKE_INSTALL_PREFIX=${placeholder "out"}"
    "-DCMAKE_INSTALL_LIBDIR=lib"
  ];
  meta = {
    homepage = "https://www.arabeyes.org/ITL";
    description = "Islamic Tools and Libraries";
    license = lib.licenses.lgpl2;
    platforms = lib.platforms.all;
    maintainer = [lib.maintainers.kmein];
  };
}
