{
  pkgs,
  lib,
  ...
}: let
  commaSep = builtins.concatStringsSep ",";
  xkbOptions = ["compose:caps" "terminate:ctrl_alt_bksp" "grp:ctrls_toggle"];
  languages = {
    deutsch = { code = "de"; variant = "T3"; };
    greek = { code = "gr"; variant = "polytonic"; };
    russian = { code = "ru"; variant = "phonetic"; };
    arabic = ../lib/keyboards/arabic;
    coptic = ../lib/keyboards/coptic;
    avestan = ../lib/keyboards/avestan;
    gothic = ../lib/keyboards/gothic;
    sanskrit = { code = "in"; variant = "san-kagapa"; };
    gujarati = {code = "in"; variant = "guj-kagapa"; };
    hebrew = {code = "il"; variant = "phonetic";};
  };
  defaultLanguage = languages.deutsch;
in {
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
          }) (lib.filterAttrs (_: value: builtins.typeOf value == "path") languages)
        ))
      ];
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
