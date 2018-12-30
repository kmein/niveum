{ pkgs, config, lib }:
let
  unstable = import <nixos-unstable> {};
  i3blocks_conf = import ./i3blocks.nix { inherit pkgs; };
  new-workspace = unstable.writers.writeDash "new-workspace" ''
    i3-msg workspace $(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
  '';
  move-to-new-workspace = unstable.writers.writeDash "new-workspace" ''
    i3-msg move container to workspace $(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
  '';
in with import ../theme.nix;
rec {
  fonts = [ "${uiFont.name} ${toString uiFont.size}" ];
  modifier = "Mod4";
  window = {
    titlebar = false;
    border = 1;
    hideEdgeBorders = "smart";
  };
  floating = {
    titlebar = false;
    border = 1;
  };
  colors =
    let scheme = { background = colorScheme.background; text = colorScheme.foreground; };
    in {
      focused = scheme // {
        border = colorScheme.background;
        indicator = colorScheme.background;
        childBorder = colorScheme.background;
      };
      focusedInactive = scheme // {
        border = colorScheme.background;
        indicator = colorScheme.background;
        childBorder = colorScheme.background;
      };
      unfocused = scheme // {
        border = colorScheme.background;
        indicator = colorScheme.background;
        childBorder = colorScheme.background;
      };
      urgent = scheme // {
        border = colorScheme.red.light;
        indicator = colorScheme.red.light;
        childBorder = colorScheme.red.light;
      };
      placeholder = scheme // {
        border = colorScheme.green.light;
        indicator = colorScheme.green.light;
        childBorder = colorScheme.green.light;
      };
    };
  bars = [];
  keybindings = {
    "${modifier}+Down" = "focus down";
    "${modifier}+Left" = "focus left";
    "${modifier}+Return" = "exec ${config.defaultApplications.terminal}";
    "${modifier}+Right" = "focus right";
    "${modifier}+Shift+Down" = "move down";
    "${modifier}+Shift+Left" = "move left";
    "${modifier}+Shift+Right" = "move right";
    "${modifier}+Shift+Up" = "move up";
    "${modifier}+Shift+c" = "reload";
    "${modifier}+Shift+n" = "rove window to workspace next";
    "${modifier}+Shift+b" = "move window to workspace prev";
    "${modifier}+Shift+q" = "kill";
    "${modifier}+Shift+r" = "restart";
    "${modifier}+Shift+w" = "exec ${pkgs.xautolock}/bin/xautolock -locknow";
    "${modifier}+Shift+x" = "exec --no-startup-id ${move-to-new-workspace}";
    "${modifier}+Shift+z" = "floating toggle";
    "${modifier}+Up" = "focus up";
    "${modifier}+a" = "exec ${pkgs.rofi}/bin/rofi -display-window — -show window";
    "${modifier}+d" = "exec ${pkgs.rofi}/bin/rofi -display-run — -show run";
    "${modifier}+e" = "layout toggle split";
    "${modifier}+f" = "fullscreen toggle";
    "${modifier}+h" = "split h";
    "${modifier}+n" = "workspace next";
    "${modifier}+b" = "workspace prev";
    "${modifier}+r" = "mode resize";
    "${modifier}+s" = "layout stacking";
    "${modifier}+t" = "exec ${config.defaultApplications.fileManager}";
    "${modifier}+v" = "split v";
    "${modifier}+w" = "layout tabbed";
    "${modifier}+x" = "exec --no-startup-id ${new-workspace}";
    "${modifier}+y" = "exec ${config.defaultApplications.browser}";
    "XF86AudioLowerVolume" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -d 5 && pkill -RTMIN+3 i3blocks";
    "XF86AudioMute" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -t && pkill -RTMIN+3 i3blocks";
    "XF86AudioRaiseVolume" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -i 5 && pkill -RTMIN+3 i3blocks";
  };
}
