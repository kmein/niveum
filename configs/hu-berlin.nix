{ config, pkgs, lib, ... }:
let
  inherit (lib.strings) fileContents;
  inherit (import <niveum/lib>) sshPort;
  eduroam = {
    identity = fileContents <secrets/eduroam/identity>;
    password = fileContents <secrets/eduroam/password>;
  };
  eduroamAuth = ''
    key_mgmt=WPA-EAP
    eap=TTLS
    proto=RSN
    identity="${eduroam.identity}"
    anonymous_identity="anonymous@wlan.hu-berlin.de"
    altsubject_match="DNS:srv1-radius.cms.hu-berlin.de;DNS:srv2-radius.cms.hu-berlin.de"
    password="${eduroam.password}"
    ca_cert="${
      pkgs.fetchurl {
        url =
          "https://www.cms.hu-berlin.de/de/dl/netze/wlan/config/eduroam/t-telesec_globalroot_class_2.pem";
        sha256 = "0if8aqd06sid7a0vw009zpa087wxcgdd2x6z2zs4pis5kvyqj2dk";
      }
    }"
    phase2="auth=PAP"
  '';
  hu-berlin-cifs-options = [
    "uid=${toString config.users.users.me.uid}"
    "gid=${toString config.users.groups.users.gid}"
    "sec=ntlmv2"
    "workgroup=german"
    "username=meinhaki"
    "password=${lib.strings.fileContents <secrets/mail/meinhaki>}"
    "noauto"
    "x-systemd.requires=hu-vpn.service"
    "x-systemd.automount"
    "x-systemd.device-timeout=1"
    "x-systemd.idle-timeout=1min"
  ];
in {
  networking.wireless.networks = {
    eduroam_5GHz.auth = eduroamAuth;
    eduroam.auth = eduroamAuth;
  };

  fileSystems."/media/hu-berlin/germpro2" = {
    device = "//hugerm31c.user.hu-berlin.de/germpro2/ling";
    fsType = "cifs";
    options = hu-berlin-cifs-options;
  };

  fileSystems."/media/hu-berlin/germhome" = {
    device = "//hugerm31c.user.hu-berlin.de/germhome/ling/meinhaki";
    fsType = "cifs";
    options = hu-berlin-cifs-options;
  };

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "hu-ip" ''
      ${pkgs.w3m}/bin/w3m -dump meineip.hu-berlin.de | head --lines=-4 | tail --lines=+3
    '')
  ];

  systemd.services.hu-vpn = {
    enable = true;
    wants = [ "network-online.target" ];
    conflicts = [ "openvpn-hu-berlin.service" ];
    script = ''
      ${pkgs.openfortivpn}/bin/openfortivpn -c ${pkgs.writeText "hu-berlin.config" ''
        host = forti-ssl.vpn.hu-berlin.de
        port = 443
        trusted-cert = 42193a913d276d9eb86217612956e1e6464d6f07bed5393a4787c87adc4bd359
        username = ${eduroam.identity}
        password = ${eduroam.password}
      ''}
    '';
  };

  systemd.services.openvpn-hu-berlin.conflicts = [ "hu-vpn.service" ];

  services.openvpn.servers.hu-berlin = {
    autoStart = false;
    authUserPass = {
      username = eduroam.identity;
      password = eduroam.password;
    };
    config = fileContents (pkgs.fetchurl {
      url =
        "https://www.cms.hu-berlin.de/de/dl/netze/vpn/openvpn/hu-berlin.ovpn";
      sha256 = "15b55aibik5460svjq2gwxrcyh6ay4k8savd6cd5lncgndmd8p8h";
    });
  };
}
