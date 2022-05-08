{
  pkgs,
  lib,
  ...
}: let
  commaSep = builtins.concatStringsSep ",";
  languages = {
    de = "T3";
    gr = "polytonic";
    ru = "phonetic";
    ara = "buckwalter";
  };
  defaultLanguage = "de";
in {
  # man 7 xkeyboard-config
  services.xserver = {
    layout = commaSep (builtins.attrNames languages);
    # T3: https://upload.wikimedia.org/wikipedia/commons/a/a9/German-Keyboard-Layout-T3-Version1-large.png
    # buckwalter: http://www.qamus.org/transliteration.htm
    xkbVariant = commaSep (builtins.attrValues languages);
    xkbOptions =
      commaSep ["compose:caps" "terminate:ctrl_alt_bksp" "grp:ctrls_toggle"];
    libinput.enable = true;
  };

  console.keyMap = "de";

  environment.systemPackages =
    lib.mapAttrsToList
    (language: variant:
      pkgs.writers.writeDashBin "kb-${language}" ''
        ${pkgs.xlibs.setxkbmap}/bin/setxkbmap ${defaultLanguage},${language} ${languages.${defaultLanguage}},${variant}
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
