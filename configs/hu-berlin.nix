{ pkgs, ... }:
let
  eduroam = (import <niveum/secrets.nix>).eduroam;
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
  networking.wireless.networks = {
    eduroam_5GHz = eduroamConfig;
    eduroam = eduroamConfig;
  };

  services.openvpn.servers = {
    hu-berlin = {
      config = ''config ${pkgs.fetchurl {
          url = https://www.cms.hu-berlin.de/de/dl/netze/vpn/openvpn/hu-berlin.ovpn;
          sha256 = "15b55aibik5460svjq2gwxrcyh6ay4k8savd6cd5lncgndmd8p8h";
        }}
        # route-nopull
        # route 141.20.0.0 255.255.0.0'';
      authUserPass = {
        username = eduroam.identity;
        password = eduroam.password;
      };
    };
  };
}
