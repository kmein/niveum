{pkgs, ...}:
# https://paste.sr.ht/~erictapen/11716989e489b600f237041b6d657fdf0ee17b34
let
  name = "dst-root-ca-x3.pem";
  certificate = pkgs.stdenv.mkDerivation {
    inherit name;
    src = builtins.toFile "${name}.sed" ''
      1,/DST Root CA X3/d
      1,/-----END CERTIFICATE-----/p
    '';
    nativeBuildInputs = with pkgs; [cacert gnused];
    phases = "installPhase";
    installPhase = ''
      ${pkgs.gnused}/bin/sed -n -f $src ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt > $out
    '';
  };
in {
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
