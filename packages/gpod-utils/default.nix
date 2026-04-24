{
  lib,
  stdenv,
  fetchFromGitHub,
  autoreconfHook,
  pkg-config,
  libgpod,
  glib,
  json_c,
  sqlite,
  ffmpeg,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "gpod-utils";
  version = "1.3.4";

  src = fetchFromGitHub {
    owner = "whatdoineed2do";
    repo = "gpod-utils";
    rev = "v${finalAttrs.version}";
    hash = "sha256-rqQhtLneVmB/qm6MY9Xaxflg0IqJvPekY7wxUomk/CI=";
  };

  patches = [ ./fix-format-string.patch ];

  nativeBuildInputs = [
    autoreconfHook
    pkg-config
  ];

  buildInputs = [
    libgpod
    glib
    json_c
    sqlite
    ffmpeg
  ];

  meta = with lib; {
    description = "Command line utilities to manage iPods using libgpod";
    longDescription = ''
      gpod-utils provides CLI tools to manage iPod content via libgpod:
        gpod-ls      - list iPod tracks as JSON (optionally into SQLite)
        gpod-cp      - copy audio/video files to iPod with auto-transcoding
        gpod-rm      - remove tracks from iPod
        gpod-tag     - edit track metadata in the iTunesDB
        gpod-extract - copy files off the iPod to your filesystem
        gpod-verify  - verify iTunesDB against actual files on device
        gpod-hashsum - compute audio-stream checksums for dedup
    '';
    homepage = "https://github.com/whatdoineed2do/gpod-utils";
    license = licenses.gpl2Only;
    maintainers = [ ];
    platforms = platforms.linux;
    mainProgram = "gpod-ls";
  };
})
