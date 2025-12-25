{
  config,
  pkgs,
  lib,
  ...
}: let
  username = "meinhak99";
  fu-defaults = let mailhost = "mail.zedat.fu-berlin.de"; in {
    imap.host = mailhost;
    imap.port = 993;
    imap.tls.enable = true;
    smtp.host = mailhost;
    smtp.port = 465;
    smtp.tls.enable = true;
    folders.drafts = "Entwürfe";
    folders.sent = "Gesendet";
    folders.trash = "Papierkorb";
  };
in {
  home-manager.users.me = {
    programs.ssh = {
      matchBlocks = {
        fu-berlin = {
          user = username;
          hostname = "login.zedat.fu-berlin.de";
          setEnv.TERM = "xterm";
        };
      };
    };
    accounts.email.accounts = {
      letos =
        lib.recursiveUpdate pkgs.lib.niveum.email.defaults
        {
          userName = "slfletos";
          address = "letos.sprachlit@hu-berlin.de";
          passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.email-password-letos.path}";
          imap.host = "mailbox.cms.hu-berlin.de";
          imap.port = 993;
          smtp.host = "mailhost.cms.hu-berlin.de";
          smtp.port = 25;
          smtp.tls.useStartTls = true;
        };
      fu =
        lib.recursiveUpdate pkgs.lib.niveum.email.defaults
        (lib.recursiveUpdate fu-defaults
          (let userName = "meinhak99"; in {
            userName = userName;
            address = "kieran.meinhardt@fu-berlin.de";
            aliases = ["${userName}@fu-berlin.de"];
            passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.email-password-meinhak99.path}";
            himalaya = {
              enable = true;
              settings.backend = "imap";
            };
          }));
    };
  };

  age.secrets = {
    email-password-meinhak99 = {
      file = ../secrets/email-password-meinhak99.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
    email-password-letos = {
      file = ../secrets/email-password-letos.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
    fu-sftp-key = {
      file = ../secrets/fu-sftp-key.age;
      owner = "root";
      group = "root";
      mode = "400";
    };
  };

  # if it fails with "connection reset by peer" run `sudo sshfs ... ... -o ...` manually
  # it needs to say 'yes' to the server's fingerprint
  system.fsPackages = [ pkgs.sshfs ];

  # https://www.zedat.fu-berlin.de/tip4u_157.pdf
  fileSystems = let
    fu-berlin-cifs-options = [
      "uid=${toString config.users.users.me.uid}"
      "gid=${toString config.users.groups.users.gid}"
      "rw"
      "nounix"
      "domain=fu-berlin"
      "noauto"
      "x-systemd.automount"
      "x-systemd.device-timeout=1"
      "x-systemd.idle-timeout=1min"
    ];

    firstCharacter = lib.strings.substring 0 1;

    home-directory-mount = user: {
      "${pkgs.lib.niveum.remoteDir}/fu/${user}/home" = {
        device = "${user}@login.zedat.fu-berlin.de:/home/${firstCharacter user}/${user}";
        fsType = "sshfs";
        options = [
          "allow_other"
          "_netdev"
          "x-systemd.automount"
          "reconnect"
          "ServerAliveInterval=15"
          "IdentityFile=${config.age.secrets.fu-sftp-key.path}"
        ];
      };
    };
  in home-directory-mount "meinhak99";

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "hu-vpn-split" ''
      ${pkgs.openfortivpn}/bin/openfortivpn \
        --password="$(cat "${config.age.secrets.email-password-letos.path}")" \
        --config=${
        pkgs.writeText "hu-berlin-split.config" ''
          host = forti-ssl.vpn.hu-berlin.de
          port = 443
          username = slfletos@split_tunnel
        ''
      }
    '')
    (pkgs.writers.writeDashBin "hu-vpn-full" ''
      ${pkgs.openfortivpn}/bin/openfortivpn \
        --password="$(cat "${config.age.secrets.email-password-letos.path}")" \
        --config=${
        pkgs.writeText "hu-berlin-full.config" ''
          host = forti-ssl.vpn.hu-berlin.de
          port = 443
          username = slfletos@tunnel_all
        ''
      }
    '')
    (pkgs.writers.writeDashBin "fu-vpn" ''
      if ${pkgs.wirelesstools}/bin/iwgetid | ${pkgs.gnugrep}/bin/grep --invert-match eduroam
      then
        # root firefox will not open login window unless root owns Xauthority
        sudo cp $XAUTHORITY /root/.Xauthority
        sudo chown root: /root/.Xauthority
        XAUTHORITY=/root/.Xauthority sudo ${pkgs.openconnect}/bin/openconnect vpn.fu-berlin.de --useragent=AnyConnect
      fi
    '')
  ];
}
