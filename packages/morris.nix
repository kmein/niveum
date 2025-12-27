{
  stdenv,
  boost,
  gtk2,
  gettext,
  intltool,
  fetchurl,
  pkg-config,
  wrapGAppsHook3,
  lib,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "morris";
  version = "0.3";
  src = fetchurl {
    url = "https://nine-mens-morris.net/data/morris-${finalAttrs.version}.tar.bz2";
    hash = "sha256-f1kOpYB1oXOAKqwb1ya0jfJA5vqxA+v8MjEZ1zPPutM=";
  };

  nativeBuildInputs = [
    pkg-config
    gettext
    intltool
    wrapGAppsHook3
  ];

  buildInputs = [
    boost
    gtk2
  ];

  # Help configure find Boost headers
  configureFlags = [
    "--with-boost=${boost.dev}"
    "--with-boost-libdir=${boost.out}/lib"
  ];

  meta = with lib; {
    description = "Nine Men's Morris game";
    homepage = "https://github.com/farindk/morris";
    license = licenses.gpl3Plus;
    platforms = platforms.linux;
  };
})
