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

  users.users.root.openssh.authorizedKeys.keys = pkgs.lib.niveum.kieran.sshKeys ++ [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPoiRIn1dBUtpApcUyGbZKN+m5KBSgKIDQjdnQ8vU0xU kfm@kibbeh" # travel laptop
  ];
}
