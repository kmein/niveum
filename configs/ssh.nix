{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (import <niveum/lib>) sshPort kieran;
  externalNetwork = import <niveum/lib/external-network.nix>;
  sshIdentity = name: "${config.users.users.me.home}/.ssh/${name}";
  ssh-passphease = lib.strings.fileContents <system-secrets/ssh/passphrase>;
in {
  services.xserver.displayManager.sessionCommands = toString (pkgs.writeScript "ssh-add" ''
    #!${pkgs.expect}/bin/expect -f
    spawn ${pkgs.openssh}/bin/ssh-add
    expect "Enter passphrase for *:"
    send "${ssh-passphrase}\n";
    expect "Identity added: *"
    interact
  '');

  programs.ssh.startAgent = true;

  users.users.me.openssh.authorizedKeys.keys = kieran.sshKeys pkgs;

  home-manager.users.me.programs.ssh = {
    enable = true;
    matchBlocks = {
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
      "fysi-dev1" = {
        hostname = "94.130.229.139";
        user = "root";
        identityFile = sshIdentity "fysiweb";
      };
      "fysi-shared0" = {
        hostname = "49.12.205.235";
        user = "root";
        identityFile = sshIdentity "fysiweb";
      };
    };
  };
}
