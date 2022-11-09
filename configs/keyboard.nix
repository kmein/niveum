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
    ara = "buckwalter";
    cop = "";
    ave = "";
    "in" = "san-kagapa";
    il = "phonetic";
  };
  defaultLanguage = "de";
in {
  # man 7 xkeyboard-config
  services.xserver = {
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
            name = "symbols/cop";
            path = pkgs.fetchurl {
              url = "http://www.moheb.de/download/cop";
              sha256 = "1l0h6aq536hyinrh0i0ia355y229bjrlibii0sya5bmqh46vycia";
            };
          }
          {
            name = "symbols/ave";
            path = pkgs.fetchurl {
              url = "https://blog.simos.info/wp-content/uploads/2010/06/avestan.txt";
              sha256 = "192zmmm3gxyhim39dsax7r87gsay2w5v2xkhwmvsfipjb60hwp5g";
            };
          }
        ])
      ];
    };
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
