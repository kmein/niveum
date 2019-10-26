{ lib, ... }:
let inherit (import <niveum/lib> { inherit lib; }) commaSep;
in {
  services.xserver = {
    layout = commaSep [ "de" "gr" "ru" ];
    xkbVariant = commaSep [ "T3" "polytonic" "phonetic_winkeys" ];
    xkbOptions = commaSep [ "compose:caps" "terminate:ctrl_alt_bksp" "grp:ctrls_toggle" ];
    libinput.enable = true;
  };

  i18n.consoleKeyMap = "de";
}
