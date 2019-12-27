{ pkgs, ... }:
# https://paste.sr.ht/~erictapen/11716989e489b600f237041b6d657fdf0ee17b34
let
  certificate = pkgs.stdenv.mkDerivation rec {
    name = "dst-root-ca-x3.pem";
    src = builtins.toFile "${name}.awk" ''
      {
        if(a > 0) { print }
      }

      /-----END CERTIFICATE-----/ { a = 0 }

      /DST Root CA X3/ { a = 1 }
    '';
    nativeBuildInputs = with pkgs; [ cacert gawk ];
    phases = "installPhase";
    installPhase = ''
      ${pkgs.gawk}/bin/awk -f $src ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt > $out
    '';
  };
in
{
  networking.wireless.networks."36C3" = {
    auth = ''
      key_mgmt=WPA-EAP
      eap=TTLS
      identity="kmein"
      password=" "
      ca_cert="${certificate}"
      altsubject_match="DNS:radius.c3noc.net"
      phase2="auth=PAP"
    '';
  };
}
