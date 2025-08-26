{
  pkgs,
  lib,
  ...
}: let

  commaSep = builtins.concatStringsSep ",";
  xkbOptions = ["compose:caps" "terminate:ctrl_alt_bksp" "grp:ctrls_toggle"];
  languages = {
    arabic = { code = "ara"; variant = "buckwalter"; }; # ../lib/keyboards/arabic;
    avestan = ../lib/keyboards/avestan;
    coptic = ../lib/keyboards/coptic;
    deutsch = { code = "de"; variant = "T3"; };
    farsi = { code = "ir"; variant = "qwerty"; };
    gothic = ../lib/keyboards/gothic;
    greek = { code = "gr"; variant = "polytonic"; };
    gujarati = {code = "in"; variant = "guj-kagapa"; };
    hebrew = {code = "il"; variant = "phonetic";};
    russian = { code = "ru"; variant = "phonetic"; };
    sanskrit = { code = "in"; variant = "san-kagapa"; };
    syriac = { code = "sy"; variant = "syc_phonetic"; };
    urdu = {code = "in"; variant = "urd-phonetic"; };
  };
  defaultLanguage = languages.deutsch;
in {
  services.libinput.enable = true;

  # man 7 xkeyboard-config
  services.xserver = {
    # exportConfiguration = true; # link /usr/share/X11 properly
    xkb.layout = defaultLanguage.code;
    # T3: https://upload.wikimedia.org/wikipedia/commons/a/a9/German-Keyboard-Layout-T3-Version1-large.png
    # buckwalter: http://www.qamus.org/transliteration.htm
    xkb.variant = defaultLanguage.variant;
    xkb.options = commaSep xkbOptions;
    xkb.extraLayouts = {
      "coptic" = {
        languages = ["cop"];
        description = "Coptic";
        symbolsFile = ../lib/keyboards/coptic;
      };
      "gothic" = {
        languages = ["got"];
        description = "Gothic";
        symbolsFile = ../lib/keyboards/gothic;
      };
      "avestan" = {
        languages = ["ave"];
        description = "Avestan";
        symbolsFile = ../lib/keyboards/avestan;
      };
      "farsi-good" = {
        languages = ["fas"];
        description = "Farsi, but good";
        symbolsFile = ../lib/keyboards/farsi;
      };
    };
  };

  environment.etc."x11-locale".source = toString pkgs.xorg.libX11 + "share/X11/locale";

  console.keyMap = "de";

  environment.systemPackages =
    lib.mapAttrsToList
    (language: settings:
    let
      code = if settings ? "code" then settings.code else language;
      variant = if settings ? "variant" then settings.variant else "";
    in
      pkgs.writers.writeDashBin "kb-${language}" ''
        ${pkgs.xorg.setxkbmap}/bin/setxkbmap ${defaultLanguage.code},${code} ${defaultLanguage.variant},${variant} ${toString (map (option: "-option ${option}") xkbOptions)}
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
