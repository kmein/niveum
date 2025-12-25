{pkgs, ...}: let
  inherit (import ../lib) sshPort kieran;
  externalNetwork = import ../lib/external-network.nix;
in {
  users.users.me.openssh.authorizedKeys.keys = kieran.sshKeys;
  programs.ssh.startAgent = true;
  services.gnome.gcr-ssh-agent.enable = false;

  home-manager.users.me = {
    # https://discourse.nixos.org/t/gnome-keyring-and-ssh-agent-without-gnome/11663
    xsession.profileExtra = ''
      eval $(${pkgs.gnome3.gnome-keyring}/bin/gnome-keyring-daemon --daemonize --components=ssh,secrets)
      export SSH_AUTH_SOCK
    '';
  };

  home-manager.users.me.programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
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
      fatteh = {
        hostname = "fatteh.r";
        user = "kfm";
        port = sshPort;
      };
    };
  };
}
