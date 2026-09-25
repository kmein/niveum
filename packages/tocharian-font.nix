{
  stdenv,
  lib,
  fetchurl,
  mupdf,
}:
stdenv.mkDerivation {
  pname = "tocharian-font";
  version = "unstable-2022-01-11";
  src = fetchurl {
    url = "https://unicode.org/L2/L2015/15236-tocharian.pdf";
    sha256 = "08bzkva9a6b2cfl38p9m22b1cf6yv27xsw6nrvq5ly5nffjm32hv";
  };
  dontUnpack = true;
  nativeBuildInputs = [ mupdf ];
  buildPhase = ''
    runHook preBuild
    mutool extract $src
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/fonts/truetype
    install font-0021.ttf $out/share/fonts/truetype/Tocharian.ttf
    runHook postInstall
  '';
  meta = {
    description = "Tocharian font by Lee Wilson";
    platforms = lib.platforms.all;
    maintainers = [ lib.maintainers.kmein ];
  };
}
