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
    xkbVariant = commaSep ["T3" "polytonic" "phonetic" "buckwalter"];
    xkbOptions =
      commaSep ["compose:caps" "terminate:ctrl_alt_bksp" "grp:ctrls_toggle"];
    libinput.enable = true;
  };

  console.keyMap = "de";

  # improve held key rate
  services.xserver.displayManager.sessionCommands = "${pkgs.xorg.xset}/bin/xset r rate 300 50";
}
