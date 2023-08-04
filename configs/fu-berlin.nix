{
  config,
  pkgs,
  ...
}: let
  username = "meinhak99";
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
  };

  systemd.user.services.fu-blackboard-calendar = {
    enable = true;
    # startAt = "*:00/10";
    after = ["vdirsyncer.service"];
    wantedBy = ["default.target"];
    script = ''
      ics_url=$(cat "$CREDENTIALS_DIRECTORY/ics-url")
      curl "$ics_url" | ${pkgs.khal}/bin/khal import -
    '';
    serviceConfig = {
      Type = "oneshot";
      Restart = "on-failure";
      LoadCredential = "ics-url:${config.age.secrets.blackboard-calendar-ics.path}";
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
