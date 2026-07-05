{
  pkgs,
  config,
  lib,
  ...
}:
let
  rofi = config.home-manager.users.me.programs.rofi.finalPackage;
in
{
  home-manager.users.me.programs.rofi = {
    enable = true;
    # stylix would set the monospace font here; rofi is UI, not a terminal
    font = lib.mkForce "${config.stylix.fonts.sansSerif.name} ${toString config.stylix.fonts.sizes.applications}";
    pass = {
      enable = true;
      extraConfig = ''
        _pwgen() {
          ${pkgs.genpass}/bin/genpass "$@"
        }

        USERNAME_field='login'
        default_user2=kmein
        help_color="#FF0000"
      ''; # help_color set by https://github.com/mrossinek/dotfiles/commit/13fc5f24caa78c8f20547bf473266879507f13bf
    };
    plugins = [ pkgs.rofi-calc ];
  };

  # use the home-manager-styled rofi instead of the niphas wrapper,
  # which would bypass the config written by home-manager/stylix
  niphas.runner.package = rofi;
  niphas.niri.settings.binds."Mod+D".spawn-sh = "${lib.getExe rofi} -show run";
}
