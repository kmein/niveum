{
  config,
  pkgs,
  ...
}:
{
  users.motd = "Welcome to ${config.networking.hostName}!";

  services.openssh = {
    enable = true;
    ports = [ pkgs.lib.niveum.machines.${config.networking.hostName}.sshPort ];
    settings = {
      PasswordAuthentication = false;
      X11Forwarding = true;
    };
  };

  users.users.root.openssh.authorizedKeys.keys = pkgs.lib.niveum.kieran.sshKeys;
}
