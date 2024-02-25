{
  pkgs,
  lib,
  ...
}: let
  commaSep = builtins.concatStringsSep ",";
  xkbOptions = ["compose:caps" "terminate:ctrl_alt_bksp" "grp:ctrls_toggle"];
  languages = {
    de = "T3";
    gr = "polytonic";
    ru = "phonetic";
    arabic = "";
    coptic = "";
    avestan = "";
    gothic = "";
    "in" = "san-kagapa";
    il = "phonetic";
  };
  defaultLanguage = "de";
in {
  # man 7 xkeyboard-config
  services.xserver = {
    exportConfiguration = true; # link /usr/share/X11 properly
    layout = "de";
    # T3: https://upload.wikimedia.org/wikipedia/commons/a/a9/German-Keyboard-Layout-T3-Version1-large.png
    # buckwalter: http://www.qamus.org/transliteration.htm
    xkbVariant = "T3";
    xkbOptions = commaSep xkbOptions;
    libinput.enable = true;
    xkbDir = pkgs.symlinkJoin {
      name = "x-keyboard-directory";
      paths = [
        "${pkgs.xkeyboard_config}/etc/X11/xkb"
        (pkgs.linkFarm "custom-x-keyboards" [
          {
            name = "symbols/arabic";
            path = ../lib/keyboards/arabic;
          }
          {
            name = "symbols/coptic";
            path = ../lib/keyboards/coptic;
          }
          {
            name = "symbols/gothic";
            path = ../lib/keyboards/gothic;
          }
          {
            name = "symbols/avestan";
            path = ../lib/keyboards/avestan;
          }
        ])
      ];
    };
  };

  environment.etc."x11-locale".source = toString pkgs.xorg.libX11 + "share/X11/locale";

  home-manager.users.me = {
    home.file.".XCompose".source = ../lib/keyboards/XCompose;
  };

  console.keyMap = "de";

  environment.systemPackages =
    lib.mapAttrsToList
    (language: variant:
      pkgs.writers.writeDashBin "kb-${language}" ''
        ${pkgs.xorg.setxkbmap}/bin/setxkbmap ${defaultLanguage},${language} ${languages.${defaultLanguage}},${variant} ${toString (map (option: "-option ${option}") xkbOptions)}
      '')
    languages;

  # improve held key rate
  services.xserver.displayManager.sessionCommands = "${pkgs.xorg.xset}/bin/xset r rate 300 50";

  systemd.user.services.gxkb = {
    wantedBy = ["graphical-session.target"];
    serviceConfig = {
      SyslogIdentifier = "gxkb";
      ExecStart = "${pkgs.gxkb}/bin/gxkb";
      Restart = "always";
      RestartSec = "15s";
      StartLimitBurst = 0;
    };
  };
}
