{
  pkgs,
  config,
  ...
}:
let
  suspend = pkgs.writers.writeDash "suspend" "${pkgs.systemd}/bin/systemctl suspend";
in
{
  services.power-action = {
    enable = true;
    plans.suspend = {
      upperLimit = 7;
      lowerLimit = 0;
      charging = false;
      action = pkgs.writers.writeDash "suspend-wrapper" ''
        /run/wrappers/bin/sudo ${suspend}
      '';
    };
    user = config.users.users.me.name;
  };

  security.sudo.extraConfig = ''
    ${config.services.power-action.user} ALL= (root) NOPASSWD: ${suspend}
  '';
}
