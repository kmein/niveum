{
  pkgs,
  lib,
  ...
}:
let
in
{
  environment.variables.TERMINAL = "alacritty";

  home-manager.users.me = {
    programs.alacritty = {
      enable = true;
      settings = {
        keyboard.bindings = [
          {
            key = "Plus";
            mods = "Control";
            action = "IncreaseFontSize";
          }
          {
            key = "Minus";
            mods = "Control";
            action = "DecreaseFontSize";
          }
          {
            key = "Key0";
            mods = "Control";
            action = "ResetFontSize";
          }
        ];
      };
    };
  };
}
