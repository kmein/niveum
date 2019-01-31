{ pkgs, lib, config, ... }:
{
  services.xserver = with import ../helpers.nix; with import ../theme.nix; {
    enable = true;
    layout = commaSep [ "de" "gr" "ru" ];
    xkbVariant = commaSep [ "T3" "polytonic" "phonetic_winkeys" ];
    xkbOptions = commaSep [ "compose:caps" "terminate:ctrl_alt_bksp" "grp:ctrls_toggle" ];
    libinput.enable = true;
    xautolock = {
      enable = true;
      killer = "${pkgs.systemd}/bin/systemctl suspend";
      locker = config.defaultApplications.locker;
      nowlocker = config.defaultApplications.locker;
      enableNotifier = true;
      notifier = ''${pkgs.libnotify}/bin/notify-send -u normal -a xautolock "Locking soon" "The screen will lock in 10 seconds."'';
    };
    displayManager.sessionCommands = ''
      ${pkgs.feh}/bin/feh --bg-fill $(find ${../art} -type f | shuf -n 1) &
      ${pkgs.dropbox-cli}/bin/dropbox start &
      ${pkgs.seafile-client}/bin/seafile-applet &
      ${pkgs.systemd}/bin/systemctl --user import-environment XDG_SESSION_PATH
      ${pkgs.lightlocker}/bin/light-locker &
      ${pkgs.openssh}/bin/ssh-add
    '';
    displayManager.lightdm.greeters.gtk = {
      enable = true;
      theme = { name = config.constants.theme.gtk.name; package = config.constants.theme.gtk.package; };
      iconTheme = { name = config.constants.theme.icon.name; package = config.constants.theme.icon.package; };
      indicators = [ "~spacer" "~host" "~spacer" "~session" "~power" ];
    };
    desktopManager.xterm.enable = false;
    windowManager.default = "i3";
    windowManager.i3.enable = true;
    xrandrHeads = {
      homeros = [ "LVDS1" { output = "HDMI1"; primary = true; } ];
      scardanelli = [ "eDP1" ];
    }.${config.networking.hostName};
  };

  i18n = {
    defaultLocale = "en_GB.UTF-8";
    consoleKeyMap = "de";
    consoleColors = with import ../theme.nix; map (c: lib.strings.removePrefix "#" c) colorPalette;
  };

  services.compton = {
    enable = true;
    shadow = true;
    menuOpacity = "0.9";
    shadowOpacity = "0.3";
  };

  services.redshift = {
    enable = true;
    latitude = "52";
    longitude = "13";
  };

  services.illum.enable = true;

  services.unclutter = {
    enable = true;
    timeout = 10;
  };

  home-manager.users.kfm = {
    gtk = {
      enable = true;
      # font = with import ../theme.nix; { package = pkgs.noto-fonts; name = uiFont.name; };
      iconTheme = config.constants.theme.icon;
      theme = config.constants.theme.gtk;
    };

    qt = {
      enable = true;
      useGtkTheme = true;
    };

    xsession.pointerCursor = config.constants.theme.cursor // { size = 16; };

    xsession.windowManager.i3 = {
      enable = true;
      config = import ../dot/i3.nix { inherit lib pkgs config; };
    };

    xresources.properties = import ../dot/xresources.nix { inherit lib; };
    programs.rofi = import ../dot/rofi.nix { inherit config; };
    services.dunst = import ../dot/dunst.nix { inherit pkgs config; };
    programs.urxvt = import ../dot/urxvt.nix { inherit pkgs; };
    programs.zathura = import ../dot/zathura.nix;
  };
}
