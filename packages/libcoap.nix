{ lib, pkg-config, fetchFromGitHub, automake, autoconf, which, libtool, stdenv, gnutls
, doxygen, asciidoc
, tls ? false, docs ? true }:
stdenv.mkDerivation {
  name = "libcoap";
  version = "unstable-2021-05-28";
  src = fetchFromGitHub {
    repo = "libcoap";
    owner = "obgm";
    rev = "62b2be4da1d0fdf4b7217487ee83dc5920174425";
    sha256 = "1igjv0hbwmakdccp5in4gw9w2p5swxdwsdx0glyna2s29sh1d37x";
    fetchSubmodules = true;
  };
  buildInputs = [ which pkg-config automake autoconf libtool ]
    ++ lib.optionals docs [ doxygen asciidoc ]
    ++ lib.optional tls gnutls;
  # preConfigure = "./autogen.sh";
  # configureFlags = lib.optional (!docs) "--disable-documentation" ++  lib.optional tls "--enable-dtls";
  configurePhase = ''
    ./autogen.sh || :
    ./configure --enable-dtls --prefix=$out
  '';
  buildPhase = "make";
  installPhase = "make install";
  meta = with lib; {
    homepage = "https://github.com/obgm/libcoap";
    description = "A CoAP (RFC 7252) implementation in C";
    platforms = platforms.linux;
    license = licenses.bsd2;
    maintainers = [ maintainers.kmein ];
  };
}
