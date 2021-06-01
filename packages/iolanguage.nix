{ lib, stdenv, cmake, python3, fetchFromGitHub }:
stdenv.mkDerivation rec {
  version = "2017.09.06";
  name = "iolanguage-${version}";
  src = fetchFromGitHub {
    owner = "IoLanguage";
    repo = "io";
    rev = "${version}";
    sha256 = "07rg1zrz6i6ghp11cm14w7bbaaa1s8sb0y5i7gr2sds0ijlpq223";
    fetchSubmodules = true;
  };
  buildInputs = [ cmake python3 ];
  preBuild = "mkdir -p build && cd build";
  buildPhase = ''
    cmake -DCMAKE_INSTALL_PREFIX=$out/ ..
    make all
  '';
  meta = with lib; {
    homepage = "https://iolanguage.org/";
    description =
      "Io programming language. Inspired by Self, Smalltalk and LISP.";
    license = licenses.bsd3;
  };
}
