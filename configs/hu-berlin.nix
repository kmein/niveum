{ config, pkgs, lib, ... }:
let
  inherit (lib.strings) fileContents;
  inherit (import <niveum/lib>) sshPort;
  eduroam = {
    identity = fileContents <secrets/eduroam/identity>;
    password = fileContents <secrets/eduroam/password>;
  };
  hu-berlin-cifs-options = [
    "uid=${toString config.users.users.me.uid}"
    "gid=${toString config.users.groups.users.gid}"
    "sec=ntlmv2"
    "workgroup=german"
    "username=meinhaki"
    "password=${lib.strings.fileContents <secrets/mail/meinhaki>}"
    "noauto"
    "x-systemd.requires=openvpn-hu-berlin.service"
    "x-systemd.automount"
    "x-systemd.device-timeout=1"
    "x-systemd.idle-timeout=1min"
  ];
in {
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
    (pkgs.writers.writePython3Bin "hu-eduroam-install"
      {
        libraries = with pkgs.python3Packages; [ distro pyopenssl dbus-python ];
        flakeIgnore = [ "E501" "E123" "W504" "E722" "F821" "E226" "E126" "E265" "W291" ];
      }
      (builtins.readFile (builtins.fetchurl {
        url = "https://www.cms.hu-berlin.de/de/dl/netze/wlan/config/eduroam/linux-installer/eduroam-linux-hub.py";
        sha256 = "19x2kvwxx13265b2hj5fjf53g0liw6dw7xf9j9cav67cswmz60kf";
      }))
    )
  ];

  systemd.services.hu-vpn = {
    enable = false;
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
