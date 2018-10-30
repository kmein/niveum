{ pkgs, ... }:
let
  eduroam = (import ../secrets.nix).eduroam;
  eduroamConfig = {
      auth = ''
        key_mgmt=WPA-EAP
        eap=TTLS
        proto=RSN
        identity="${eduroam.identity}"
        anonymous_identity="anonymous@wlan.hu-berlin.de"
        altsubject_match="DNS:srv1-radius.cms.hu-berlin.de;DNS:srv2-radius.cms.hu-berlin.de"
        password="${eduroam.password}"
        ca_cert="${pkgs.fetchurl {
          url = https://www.cms.hu-berlin.de/de/dl/netze/wlan/config/eduroam/t-telesec_globalroot_class_2.pem;
          sha256 = "b30989fd9e45c74bf417df74d1da639d1f04d4fd0900be813a2d6a031a56c845";
        }}"
        phase2="auth=PAP"
      '';
    };
in {
  networking.hosts = {
    "192.168.178.27" = [ "printer.local" ];
  };

  networking.wireless.enable = true;
  networking.wireless.userControlled.enable = true;
  networking.wireless.networks = {
    Aether = { pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a"; };
    eduroam_5GHz = eduroamConfig;
    eduroam = eduroamConfig;
    "Asoziales Netzwerk" = { pskRaw = "8e234041ec5f0cd1b6a14e9adeee9840ed51b2f18856a52137485523e46b0cb6"; };
    "c-base-public" = {};
    "FlixBus" = {};
    "FlixBus Wi-Fi" = {};
  };

  services.openvpn.servers = {
    hu-berlin = {
      config = ''config ${pkgs.fetchurl {
          url = https://www.cms.hu-berlin.de/de/dl/netze/vpn/openvpn/hu-berlin.ovpn;
          sha256 = "d61a644b1e8bd313a8c4bdf1024d8445d56d1fb4a85d2574d597fc020c4901dc";
        }}
        route-nopull
        route 141.20.0.0 255.255.0.0'';
      authUserPass = {
        username = eduroam.identity;
        password = eduroam.password;
      };
    };
  };
}
