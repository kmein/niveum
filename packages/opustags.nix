{
  lib,
  stdenv,
  cmake,
  pkg-config,
  libogg,
  fetchFromGitHub,
}:
stdenv.mkDerivation (finalAttrs: {
  name = "opustags";
  version = "1.3.0";

  src = fetchFromGitHub {
    owner = "fmang";
    repo = "opustags";
    rev = finalAttrs.version;
    sha256 = "09z0cdg20algaj2yyhfz3hxh1biwjjvzx1pc2vdc64n8lkswqsc1";
  };

  cmakeFlags = [
    "-DCMAKE_INSTALL_PREFIX=$out"
  ];

  doCheck = true;

  buildInputs = [ libogg ];

  nativeBuildInputs = [
    cmake
    pkg-config
  ];

  meta = with lib; {
    homepage = "https://github.com/fmang/opustags";
    description = "Ogg Opus tags editor";
    platforms = platforms.all;
  };
})
