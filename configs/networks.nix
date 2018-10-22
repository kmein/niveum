let
  eduroam = (import ../secrets.nix).eduroam;
  eduroamConfig = {
      auth = ''
        key_mgmt=WPA-EAP
        eap=TTLS
        proto=RSN
        identity="${eduroam.identity}"
        anonymous_identity="anonymous@cms.hu-berlin.de"
        password="${eduroam.password}"
        ca_cert=${builtins.readFile ../deutsche-telekom-root-ca-2.crt}
        phase2="auth=PAP"
      '';
        #ca_cert="${builtins.fetchurl https://www.cms.hu-berlin.de/de/dl/netze/wlan/config/eduroam/deutsche-telekom-root-ca-2.crt}"
    };
in {
  networking.hosts = {
    "192.168.178.27" = [ "printer.local" ];
  };

  networking.wireless.enable = true;
  networking.wireless.userControlled.enable = true;
  networking.wireless.networks = {
    Aether = { pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a"; };
    "Asoziales Netzwerk" = { pskRaw = "8e234041ec5f0cd1b6a14e9adeee9840ed51b2f18856a52137485523e46b0cb6"; };
    "c-base-public" = {};
    "FlixBus" = {};
    "FlixBus Wi-Fi" = {};
    eduroam = eduroamConfig;
    eduroam_5GHz = eduroamConfig;
  };
}
