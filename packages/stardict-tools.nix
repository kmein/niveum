{
  stdenv,
  autoreconfHook,
  pkg-config,
  which,
  libtool,
  glib,
  zlib,
  gtk3,
  libmysqlclient,
  pcre,
  libxml2,
  gnused,
  fetchFromGitHub,
}:
stdenv.mkDerivation {
  name = "stardict-tools";
  nativeBuildInputs = [
    autoreconfHook
    pkg-config
    which
    libtool
  ];
  buildInputs = [
    glib
    zlib
    gtk3
    libmysqlclient
    pcre
    libxml2
  ];
  buildPhase = "make";
  configureFlags = [ "--disable-dict" ];
  env.NIX_CFLAGS_COMPILE = toString [
    "-Wno-error=format-security"
  ];
  postPatch = ''
    substituteInPlace tools/src/Makefile.am \
      --replace-fail noinst_PROGRAMS bin_PROGRAMS
  '';
  installFlags = [ "INSTALL_PREFIX=$(out)" ];
  autoreconfPhase = ''
    patchShebangs ./autogen.sh
    ./autogen.sh
  '';
  installPhase = ''
    mkdir $out
    make install
  '';
  doCheck = true;
  src = fetchFromGitHub {
    owner = "huzheng001";
    repo = "stardict-3";
    rev = "96b96d89eab5f0ad9246c2569a807d6d7982aa84";
    hash = "sha256-zmqp2maKv2JZ5fwMVE7gIOg0BKdEKZ4UvTLC0suuBRw=";
  };
}
