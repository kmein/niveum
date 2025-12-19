{
  networking.firewall.allowedTCPPorts = [ 22 ];

  containers.nethack = {
    autoStart = true;

    forwardPorts = [
      {
        containerPort = 22;
        hostPort = 22;
      }
    ];

    config =
      { pkgs, ... }:
      {
        system.stateVersion = "25.11";

        networking.hostName = "nethack";
        services.openssh.enable = true;

        environment.systemPackages = [ pkgs.nethack ];

        programs.tmux.enable = true;
        programs.tmux.extraConfig = ''
          set -g mouse on
          set -g allow-rename off
          set -g detach-on-destroy off

          unbind-key C-b
          set -g prefix None
        '';

        users.users.nethack = {
          isNormalUser = true;
          home = "/home/nethack";
          createHome = true;
          shell = pkgs.bash;
          openssh.authorizedKeys.keys = [
            "ssh-ed25519 AAAA...yourkey"
            "ssh-ed25519 AAAA...friendkey"
          ];
        };

        services.openssh.settings = {
          PasswordAuthentication = false;
          PermitRootLogin = "no";
        };

        services.openssh.extraConfig = ''
          Match User nethack
            ForceCommand ${pkgs.tmux}/bin/tmux attach -t nethack || \
                         ${pkgs.tmux}/bin/tmux new -s nethack ${pkgs.nethack}/bin/nethack
            AllowTcpForwarding no
            X11Forwarding no
            PermitTTY yes
        '';
      };
  };
}
