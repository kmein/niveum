{ pkgs, config, ... }:
let
  suspend = pkgs.writers.writeDash "suspend" "${pkgs.systemd}/bin/systemctl suspend";
in
{
  imports = [ <stockholm/krebs/3modules/power-action.nix> ];

  krebs.power-action = {
    enable = true;
    plans.suspend = {
      upperLimit = 3;
      lowerLimit = 0;
      charging = false;
      action = pkgs.writeDash "suspend-wrapper" ''
        /run/wrappers/bin/sudo ${suspend}
      '';
    };
    user = config.users.users.me.name;
  };

  security.sudo.extraConfig = ''
    ${config.krebs.power-action.user} ALL= (root) NOPASSWD: ${suspend}
  '';
}
