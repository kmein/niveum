{ lib, fetchFromGitHub, automake, autoconf, which, libtool, stdenv, gnutls
, tls ? false }:
stdenv.mkDerivation {
  name = "libcoap";
  src = fetchFromGitHub {
    repo = "libcoap";
    owner = "obgm";
    rev = "25863042ae1e95138776f65c772f9fa6ce60495c";
    sha256 = "1nic584jwkndg0w831h0fnxk0zx0apf2lw5md079m3di7zcxs6bw";
    fetchSubmodules = true;
  };
  buildInputs = [ automake autoconf which libtool ] ++ lib.optional tls gnutls;
  preConfigure = "./autogen.sh";
  configureFlags = [ "--disable-documentation" "--disable-shared" ]
    ++ lib.optional tls "--enable-dtls";
  meta = with stdenv.lib; {
    homepage = "https://github.com/obgm/libcoap";
    description = "A CoAP (RFC 7252) implementation in C";
    platforms = platforms.linux;
    license = licenses.bsd2;
    maintainers = [ maintainers.kmein ];
  };
}
