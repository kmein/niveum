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
    settings =
      # <name>.niveum aliases for every sshable machine: the ProxyCommand runs
      # the try-connect probe (LAN/public/hyprspace/retiolum race, Tor
      # fallback), so deploy-rs, ssh, and scp all get multi-route fallback via
      # one stable name. Multiplexing keys the control socket on the short
      # alias (%C hashes it), which sidesteps the 108-byte socket path limit
      # that .onion hostnames used to hit, and makes the discovery probe run
      # once per ControlPersist window rather than once per connection.
      lib.mapAttrs' (
        name: machine:
        lib.nameValuePair "${name}.niveum" {
          user = "root";
          port = machine.sshPort;
          proxyCommand = "${pkgs.niveum-proxy}/bin/niveum-proxy-${name} %p";
          controlMaster = "auto";
          controlPath = "~/.ssh/control-%C";
          controlPersist = "10m";
        }
      ) (lib.filterAttrs (_: machine: machine ? sshPort) pkgs.lib.niveum.machines)
      // {
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
        "*.onion".proxyCommand = "nc -xlocalhost:9050 %h %p";
      };
  };
}
