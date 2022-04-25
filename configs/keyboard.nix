{
  pkgs,
  lib,
  ...
}: let
  commaSep = builtins.concatStringsSep ",";
in {
  # man 7 xkeyboard-config
  services.xserver = {
    layout = commaSep ["de" "gr" "ru" "ara"];
    # T3: https://upload.wikimedia.org/wikipedia/commons/a/a9/German-Keyboard-Layout-T3-Version1-large.png
    # buckwalter: http://www.qamus.org/transliteration.htm
    xkbVariant = commaSep ["T3" "polytonic" "phonetic" "buckwalter"];
    xkbOptions =
      commaSep ["compose:caps" "terminate:ctrl_alt_bksp" "grp:ctrls_toggle"];
    libinput.enable = true;
  };

  console.keyMap = "de";

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
