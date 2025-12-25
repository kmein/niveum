{ pkgs, lib, ... }:
{
  users.users.me.openssh.authorizedKeys.keys = pkgs.lib.niveum.kieran.sshKeys;
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
        port = pkgs.lib.niveum.sshPort;
      };
      makanek = {
        hostname = pkgs.lib.niveum.externalNetwork.makanek;
        user = "root";
        port = pkgs.lib.niveum.sshPort;
      };
      ful = {
        hostname = pkgs.lib.niveum.externalNetwork.ful;
        user = "root";
        port = pkgs.lib.niveum.sshPort;
      };
      tahina = {
        hostname = "tahina.r";
        user = "root";
        port = pkgs.lib.niveum.sshPort;
      };
      tabula = {
        hostname = "tabula.r";
        user = "root";
        port = pkgs.lib.niveum.sshPort;
      };
      manakish = {
        hostname = "manakish.r";
        user = "kfm";
        port = pkgs.lib.niveum.sshPort;
      };
      kabsa = {
        hostname = "kabsa.r";
        user = "kfm";
        port = pkgs.lib.niveum.sshPort;
      };
      fatteh = {
        hostname = "fatteh.r";
        user = "kfm";
        port = pkgs.lib.niveum.sshPort;
      };
    };
  };
}
