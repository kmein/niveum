{
  stdenv,
  lib,
  fetchFromGitHub,
  curl,
  cmake,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "dawn-edtior";
  version = "0.1.3";
  src = fetchFromGitHub {
    owner = "andrewmd5";
    repo = "dawn";
    rev = "v${finalAttrs.version}";
    fetchSubmodules = true;
    hash = "sha256-A3wsBHrlW7sKmDtDrmmToNTtPHekbNk/wii9fjdZgcM=";
  };
  postInstall = ''
    rm -rf $out/lib $out/include $out/bin/pcre2-config
  '';
  buildInputs = [ curl ];
  nativeBuildInputs = [ cmake ];
  meta = {
    description = "A distraction-free writing environment";
    homepage = "https://github.com/andrewmd5/dawn";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.kmein ];
    platforms = lib.platforms.all;
    mainProgram = "dawn";
  };
})
