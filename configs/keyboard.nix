{
  pkgs,
  lib,
  ...
}:
let

  commaSep = builtins.concatStringsSep ",";
  xkbOptions = [
    "compose:caps"
    "terminate:ctrl_alt_bksp"
    "grp:ctrls_toggle"
  ];
  languages = {
    deutsch = {
      code = "de";
      variant = "T3";
    };
    greek = {
      code = "gr";
      variant = "polytonic";
    };
    russian = {
      code = "ru";
      variant = "phonetic";
    };
    arabic = {
      code = "ara";
      variant = "buckwalter";
    }; # ../lib/keyboards/arabic;
    coptic = ../lib/keyboards/coptic;
    avestan = ../lib/keyboards/avestan;
    gothic = ../lib/keyboards/gothic;
    farsi = {
      code = "ir";
      variant = "qwerty";
    };
    syriac = {
      code = "sy";
      variant = "syc_phonetic";
    };
    sanskrit = {
      code = "in";
      variant = "san-kagapa";
    };
    gujarati = {
      code = "in";
      variant = "guj-kagapa";
    };
    urdu = {
      code = "in";
      variant = "urd-phonetic";
    };
    hebrew = {
      code = "il";
      variant = "phonetic";
    };
  };
  defaultLanguage = languages.deutsch;
in
{
  services.libinput.enable = true;

  # man 7 xkeyboard-config
  services.xserver = {
    exportConfiguration = true; # link /usr/share/X11 properly
    xkb.layout = defaultLanguage.code;
    # T3: https://upload.wikimedia.org/wikipedia/commons/a/a9/German-Keyboard-Layout-T3-Version1-large.png
    # buckwalter: http://www.qamus.org/transliteration.htm
    xkb.variant = defaultLanguage.variant;
    xkb.options = commaSep xkbOptions;
    xkb.dir = pkgs.symlinkJoin {
      name = "x-keyboard-directory";
      paths = [
        "${pkgs.xkeyboard_config}/etc/X11/xkb"
        (pkgs.linkFarm "custom-x-keyboards" (
          lib.mapAttrsToList (name: value: {
            name = "symbols/${name}";
            path = value;
          }) (lib.filterAttrs (_: value: !(value ? "code")) languages)
          ++ [
            {
              name = "symbols/ir";
              path = ../lib/keyboards/farsi;
            }
          ]
        ))
      ];
    };
  };

  environment.etc."x11-locale".source = toString pkgs.xorg.libX11 + "share/X11/locale";

  home-manager.users.me = {
    home.file =
      lib.mapAttrs' (name: path: lib.nameValuePair ".xkb/symbols/${name}" { source = path; })
        (lib.filterAttrs (_: value: !(value ? "code")) languages) // {
          ".xkb/symbols/ir".source = ../lib/keyboards/farsi;
        };
  };

  console.keyMap = "de";

  environment.systemPackages = lib.mapAttrsToList (
    language: settings:
    let
      code = if settings ? "code" then settings.code else language;
      variant = if settings ? "variant" then settings.variant else "";
    in
    pkgs.writers.writeDashBin "kb-${language}" ''
      if [ -z $SWAYSOCK ]; then
        ${pkgs.xorg.setxkbmap}/bin/setxkbmap ${defaultLanguage.code},${code} ${defaultLanguage.variant},${variant} ${
          toString (map (option: "-option ${option}") xkbOptions)
        }
      else
        swaymsg -s $SWAYSOCK 'input * xkb_layout "${defaultLanguage.code},${code}"'
        swaymsg -s $SWAYSOCK 'input * xkb_variant "${defaultLanguage.variant},${variant}"'
        swaymsg -s $SWAYSOCK 'input * xkb_options "${lib.concatStringsSep "," xkbOptions}"'
      fi
    ''
  ) languages;

  # improve held key rate
  services.xserver.displayManager.sessionCommands = "${pkgs.xorg.xset}/bin/xset r rate 300 50";

  systemd.user.services.gxkb = {
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      SyslogIdentifier = "gxkb";
      ExecStart = "${pkgs.gxkb}/bin/gxkb";
      Restart = "always";
      RestartSec = "15s";
      StartLimitBurst = 0;
    };
  };
}
