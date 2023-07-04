{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (lib.strings) fileContents;
  inherit (import ../lib) sshPort;
  eduroam = {
    identity = fileContents <secrets/eduroam/identity>;
    password = fileContents <secrets/eduroam/password>;
  };
  hu-berlin-cifs-options = [
    "uid=${toString config.users.users.me.uid}"
    "gid=${toString config.users.groups.users.gid}"
    "sec=ntlmv2"
    "workgroup=german"
    "credentials=${config.age.secrets.cifs-credentials-hu-berlin.path}"
    "noauto"
    # "x-systemd.requires=hu-vpn.service"
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

  age.secrets.cifs-credentials-hu-berlin.file = ../secrets/cifs-credentials-hu-berlin.age;

  home-manager.users.me.programs.ssh = {
    matchBlocks = {
      "alew.hu-berlin.de" = {
        user = "centos";
        hostname = "141.20.187.219";
      };
    };
  };

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "hu-ip" ''
      ${pkgs.w3m}/bin/w3m -dump meineip.hu-berlin.de | head --lines=-4 | tail --lines=+3
    '')
    (
      pkgs.writers.writePython3Bin "hu-eduroam-install"
      {
        libraries = with pkgs.python3Packages; [distro pyopenssl dbus-python];
        flakeIgnore = ["E501" "E123" "W504" "E722" "F821" "E226" "E126" "E265" "W291"];
      }
      (builtins.readFile (builtins.fetchurl {
        url = "https://www.cms.hu-berlin.de/de/dl/netze/wlan/config/eduroam/linux-installer/eduroam-linux-hub.py";
        sha256 = "19x2kvwxx13265b2hj5fjf53g0liw6dw7xf9j9cav67cswmz60kf";
      }))
    )
  ];

  # ref https://github.com/NixOS/nixpkgs/issues/231038#issuecomment-1591888919
  environment.etc."ppp/options".text = "ipcp-accept-remote";

  systemd.services.hu-vpn = {
    enable = true;
    wants = ["network-online.target"];
    serviceConfig.LoadCredential = "password:${config.age.secrets.email-password-meinhark.path}";
    script = ''
      if ${pkgs.wirelesstools}/bin/iwgetid | ${pkgs.gnugrep}/bin/grep --invert-match eduroam
      then
        ${pkgs.openfortivpn}/bin/openfortivpn \
          --password="$(cat "$CREDENTIALS_DIRECTORY/password")" \
          --config=${
        pkgs.writeText "hu-berlin.config" ''
          host = forti-ssl.vpn.hu-berlin.de
          port = 443
          username = meinhark
        ''
      }
      fi
    '';
  };
}
