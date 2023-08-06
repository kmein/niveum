{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (import ../lib/email.nix) defaults;
  sshIdentity = name: "${config.users.users.me.home}/.ssh/${name}";
in {
  age.secrets = {
    email-password-fysi = {
      file = ../secrets/email-password-fysi.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
  };

  home-manager.users.me = {
    accounts.email.accounts = {
      fysi =
        lib.recursiveUpdate defaults
        rec {
          address = "kieran@fysi.tech";
          userName = address;
          passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.email-password-fysi.path}";
          flavor = "fastmail.com";
        };
    };

    programs.ssh.matchBlocks = rec {
      "nextcloud.fysi.dev" = {
        hostname = "116.203.82.203";
        user = "root";
      };
      "lingua.miaengiadina.ch" = {
        hostname = "135.181.85.233";
        user = "root";
      };
      "cms-dev.woc2023.app".identityFile = sshIdentity "fysiweb";
      "cms-master.woc2023.app".identityFile = sshIdentity "fysiweb";
      "fysi-dev1" = {
        hostname = "94.130.229.139";
        user = "root";
        identityFile = sshIdentity "fysiweb";
      };
      ${fysi-dev1.hostname} = fysi-dev1;
      "fysi-shared0" = {
        hostname = "49.12.205.235";
        user = "root";
        identityFile = sshIdentity "fysiweb";
      };
    };
  };
}
