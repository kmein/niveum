{
  config,
  pkgs,
  lib,
  ...
}: let
  username = "meinhak99";
  inherit (import ../lib/email.nix) defaults;
  fu-defaults = rec {
    imap.host = "mail.zedat.fu-berlin.de";
    smtp.host = imap.host;
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
      fu-student =
        lib.recursiveUpdate defaults
        (lib.recursiveUpdate fu-defaults
          rec {
            userName = "meinhak99";
            address = "kieran.meinhardt@fu-berlin.de";
            aliases = ["${userName}@fu-berlin.de"];
            passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.email-password-meinhak99.path}";
          });
    };
  };

  systemd.user.services.fu-blackboard-calendar = {
    enable = true;
    # startAt = "*:00/10";
    after = ["vdirsyncer.service"];
    wantedBy = ["default.target"];
    script = ''
      ${pkgs.curl}/bin/curl $(cat ${config.age.secrets.blackboard-calendar-ics.path}) | ${pkgs.khal}/bin/khal import -
    '';
    serviceConfig = {
      Type = "oneshot";
      Restart = "on-failure";
    };
  };

  age.secrets = {
    email-password-meinhak99 = {
      file = ../secrets/email-password-meinhak99.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
    blackboard-calendar-ics = {
      file = ../secrets/blackboard-calendar-ics.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
  };

  systemd.services.fu-vpn = {
    enable = true;
    wants = ["network-online.target"];
    serviceConfig.LoadCredential = "password:${config.age.secrets.email-password-meinhak99.path}";
    script = ''
      if ${pkgs.wirelesstools}/bin/iwgetid | ${pkgs.gnugrep}/bin/grep --invert-match eduroam
      then
        cat "$CREDENTIALS_DIRECTORY/password" | ${pkgs.openconnect}/bin/openconnect vpn.fu-berlin.de --user ${username} --passwd-on-stdin
      fi
    '';
  };
}
