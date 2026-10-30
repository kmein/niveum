{
  stdenv,
  fetchurl,
}:
stdenv.mkDerivation {
  pname = "text2pdf";
  version = "1.1";
  src = fetchurl {
    # eprg.org now answers 403 (Cloudflare) to non-browser clients
    url = "https://web.archive.org/web/20230528094058id_/http://www.eprg.org/pdfcorner/text2pdf/text2pdf.c";
    sha256 = "002nyky12vf1paj7az6j6ra7lljwkhqzz238v7fyp7sfgxw0f7d1";
  };
  dontUnpack = true;
  buildPhase = ''
    runHook preBuild
    $CC -o text2pdf $src
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall
    install -Dm755 text2pdf $out/bin/text2pdf
    runHook postInstall
  '';
}
