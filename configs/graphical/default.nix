{
  pkgs,
  lib,
  config,
  ...
}:
let
  stylixColors = config.lib.stylix.colors;
in
{
  programs.hyprland = {
    enable = true;
    withUWSM = true;
    xwayland.enable = true;
    package = pkgs.hyprland;
    portalPackage = pkgs.xdg-desktop-portal-hyprland;
  };

  programs.ydotool.enable = true;

  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-hyprland
      pkgs.xdg-desktop-portal-gtk
    ];
    config.common.default = "*";
  };

  services.dbus = {
    implementation = "broker";
    # needed for GNOME services outside of GNOME (?)
    packages = [ pkgs.gcr ];
  };

  environment.systemPackages = [
    pkgs.xdg-desktop-portal
    pkgs.xdg-desktop-portal-hyprland
  ];

  programs.regreet =
    let
      wallpaper =
        pkgs.runCommand "textured-monochrome-wallpaper.png"
          {
            buildInputs = [ pkgs.imagemagick ];
          }
          ''
            magick -size 2560x1440 plasma:fractal \
              -colorspace Gray \
              -normalize \
              -fill ${lib.escapeShellArg config.lib.stylix.colors.withHashtag.base00} -colorize 100% \
              -attenuate 0.15 +noise Gaussian \
              $out
          '';
    in
    {
      enable = true;
      settings = {
        background = {
          path = wallpaper;
          fit = "Fill";
        };
        appearance.greeting_msg = "स्वागतम्";
        widget.clock.format = "%F %H:%M";
      };
      font = {
        inherit (config.stylix.fonts.sansSerif) name;
        size = config.stylix.fonts.sizes.applications;
      };
      iconTheme = {
        inherit (config.home-manager.users.me.gtk.iconTheme) package name;
      };
    };

  home-manager.users.me = import ./home-manager.nix {
    inherit lib pkgs config;
  };
}
