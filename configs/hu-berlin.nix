{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (import ../lib/email.nix) defaults pronouns;
  inherit (import ../lib) remoteDir;
  hu-defaults = {
    imap.host = "mailbox.cms.hu-berlin.de";
    imap.port = 993;
    smtp.host = "mailhost.cms.hu-berlin.de";
    smtp.port = 25;
    smtp.tls.useStartTls = true;
  };
  hu-berlin-cifs-options = [
    "uid=${toString config.users.users.me.uid}"
    "gid=${toString config.users.groups.users.gid}"
    "sec=ntlmv2"
    "workgroup=german"
    "credentials=${config.age.secrets.cifs-credentials-hu-berlin.path}"
    "noauto"
    "x-systemd.automount"
    "x-systemd.device-timeout=1"
    "x-systemd.idle-timeout=1min"
  ];
in {
  fileSystems."${remoteDir}/hu-berlin/germpro2" = {
    device = "//hugerm31c.user.hu-berlin.de/germpro2/ling";
    fsType = "cifs";
    options = hu-berlin-cifs-options;
  };

  fileSystems."${remoteDir}/hu-berlin/germhome" = {
    device = "//hugerm31c.user.hu-berlin.de/germhome/ling/meinhaki";
    fsType = "cifs";
    options = hu-berlin-cifs-options;
  };

  age.secrets = {
    cifs-credentials-hu-berlin.file = ../secrets/cifs-credentials-hu-berlin.age;
    email-password-meinhaki = {
      file = ../secrets/email-password-meinhaki.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
    email-password-dslalewa = {
      file = ../secrets/email-password-dslalewa.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
  };

  home-manager.users.me = {
    programs.ssh = {
      matchBlocks = {
        "alew.hu-berlin.de" = {
          user = "centos";
          hostname = "141.20.187.219";
        };
      };
    };

    accounts.email.accounts = rec {
      hu-employee =
        lib.recursiveUpdate defaults
        (lib.recursiveUpdate hu-defaults
          rec {
            userName = "meinhaki";
            address = "kieran.meinhardt@hu-berlin.de";
            aliases = ["${userName}@hu-berlin.de"];
            passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.email-password-meinhaki.path}";
            aerc.extraAccounts.signature-file = toString (pkgs.writeText "signature" signature.text);
            himalaya = {
              enable = true;
              backend = "imap";
              sender = "smtp";
            };
            signature = {
              showSignature = "append";
              text = ''
                ${defaults.realName}
                ${pronouns}
                Studentische Hilfskraft / Administrator ALEW
                Humboldt-Universität zu Berlin

                Telefon: +49 (0)30 2093 9634
                Raum 3.212, Dorotheenstraße 24, 10117 Berlin-Mitte
                https://alew.hu-berlin.de
              '';
            };
          });
      hu-admin =
        lib.recursiveUpdate defaults
        (lib.recursiveUpdate hu-defaults
          rec {
            userName = "dslalewa";
            address = "admin.alew.vglsprwi@hu-berlin.de";
            himalaya = {
              enable = true;
              backend = "imap";
              sender = "smtp";
            };
            aliases = ["${userName}@hu-berlin.de"];
            passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.email-password-dslalewa.path}";
            inherit (hu-employee) signature;
            aerc.extraAccounts.signature-file = toString (pkgs.writeText "signature" signature.text);
          });
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
    serviceConfig.LoadCredential = "password:${config.age.secrets.email-password-meinhaki.path}";
    script = ''
      ${pkgs.openfortivpn}/bin/openfortivpn \
        --password="$(cat "$CREDENTIALS_DIRECTORY/password")" \
        --config=${
        pkgs.writeText "hu-berlin.config" ''
          host = forti-ssl.vpn.hu-berlin.de
          port = 443
          username = meinhaki
        ''
      }
    '';
  };
}
