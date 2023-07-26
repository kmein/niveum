{pkgs, ...}: {
  home-manager.users.me.programs.rofi = {
    enable = true;
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
    plugins = [pkgs.rofi-calc];
  };
}
