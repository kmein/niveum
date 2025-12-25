{
  config,
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
    };
    coptic = ./coptic;
    avestan = ./avestan;
    gothic = ./gothic;
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
    exportConfiguration = lib.mkForce true; # link /usr/share/X11 properly
    xkb.layout = defaultLanguage.code;
    # T3: https://upload.wikimedia.org/wikipedia/commons/a/a9/German-Keyboard-Layout-T3-Version1-large.png
    # buckwalter: http://www.qamus.org/transliteration.htm
    xkb.variant = defaultLanguage.variant;
    xkb.options = commaSep xkbOptions;
    xkb.extraLayouts = {
      coptic = {
        languages = [ "cop" ];
        description = "Coptic is the latest stage of the Egyptian language and was used by Egyptian Christians. The Coptic script is based on the Greek alphabet with some letters borrowed from Demotic Egyptian.";
        symbolsFile = ./coptic;
      };
      avestan = {
        languages = [ "ave" ];
        description = "Avestan is an ancient Iranian language known primarily from its use in the sacred texts of Zoroastrianism, the Avesta. It is an Indo-Iranian language that was spoken in ancient Persia.";
        symbolsFile = ./avestan;
      };
      gothic = {
        languages = [ "got" ];
        description = "Gothic is an extinct East Germanic language that was spoken by the Goths. It is known primarily from the Codex Argenteus, a 6th-century manuscript containing a translation of the Bible into Gothic.";
        symbolsFile = ./gothic;
      };
      farsi = {
        languages = [ "fas" ];
        description = "Farsi, also known as Persian, is an Indo-Iranian language spoken primarily in Iran, Afghanistan (where it is known as Dari), and Tajikistan (where it is called Tajik). It has a rich literary tradition and is written in a modified Arabic script.";
        symbolsFile = ./farsi;
      };
    };
  };

  environment.etc."x11-locale".source = toString pkgs.xorg.libX11 + "share/X11/locale";

  home-manager.users.me = {
    home.file = {
      ".XCompose".source = ./XCompose;
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
  ) (languages // config.services.xserver.xkb.extraLayouts);

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
