{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (import ../lib) sshPort kieran;
  externalNetwork = import ../lib/external-network.nix;
  sshIdentity = name: "${config.users.users.me.home}/.ssh/${name}";
in {
  users.users.me.openssh.authorizedKeys.keys = kieran.sshKeys pkgs;

  home-manager.users.me = {
    services.gpg-agent = rec {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtlSsh = 2 * 60 * 60;
      maxCacheTtlSsh = 4 * defaultCacheTtlSsh;
      sshKeys = [
        "568047C91DE03A23883E340F15A9C24D313E847C"
      ];
    };
  };

  home-manager.users.me.programs.ssh = {
    enable = true;
    matchBlocks = rec {
      "github.com" = {
        hostname = "ssh.github.com";
        port = 443;
      };
      zaatar = {
        hostname = "zaatar.r";
        user = "root";
        port = sshPort;
      };
      makanek = {
        hostname = externalNetwork.makanek;
        user = "root";
        port = sshPort;
      };
      ful = {
        hostname = externalNetwork.ful;
        user = "root";
        port = sshPort;
      };
      tahina = {
        hostname = "tahina.r";
        user = "root";
        port = sshPort;
      };
      tabula = {
        hostname = "tabula.r";
        user = "root";
        port = sshPort;
      };
      manakish = {
        hostname = "manakish.r";
        user = "kfm";
        port = sshPort;
      };
      kabsa = {
        hostname = "kabsa.r";
        user = "kfm";
        port = sshPort;
      };
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
