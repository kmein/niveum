pkgs: defaultApplications:
let theme = import ../theme.nix; in
with theme;
''
set $mod Mod4

font pango:${uiFont.name} ${toString uiFont.size}
floating_modifier $mod

hide_edge_borders both
new_window pixel 1
new_float  pixel 1

bindsym $mod+Return exec ${defaultApplications.terminal}
bindsym $mod+y exec ${defaultApplications.webBrowser}
bindsym $mod+t exec ${defaultApplications.fileManager}
bindsym $mod+Shift+w exec ${pkgs.xautolock}/bin/xautolock -locknow
bindsym $mod+Shift+q kill
bindsym $mod+Left focus left
bindsym $mod+Down focus down
bindsym $mod+Up focus up
bindsym $mod+Right focus right
bindsym $mod+p workspace prev
bindsym $mod+n workspace next
bindsym $mod+Shift+Left move left
bindsym $mod+Shift+Down move down
bindsym $mod+Shift+Up move up
bindsym $mod+Shift+Right move right
bindsym $mod+h split h
bindsym $mod+v split v
bindsym $mod+f fullscreen toggle
bindsym $mod+s layout stacking
bindsym $mod+w layout tabbed
bindsym $mod+e layout toggle split
bindsym $mod+Shift+z floating toggle
bindsym $mod+Shift+c reload
bindsym $mod+Shift+r restart
bindsym $mod+d exec ${pkgs.rofi}/bin/rofi -display-run — -show run

set $WS1 1
set $WS2 2
set $WS3 3
set $WS4 4
set $WS5 5
set $WS6 6
set $WS7 7
set $WS8 8
set $WS9 9
set $WS10 10
bindsym $mod+0 workspace $WS10
bindsym $mod+1 workspace $WS1
bindsym $mod+2 workspace $WS2
bindsym $mod+3 workspace $WS3
bindsym $mod+4 workspace $WS4
bindsym $mod+5 workspace $WS5
bindsym $mod+6 workspace $WS6
bindsym $mod+7 workspace $WS7
bindsym $mod+8 workspace $WS8
bindsym $mod+9 workspace $WS9
bindsym $mod+Shift+0 move container to workspace $WS10
bindsym $mod+Shift+1 move container to workspace $WS1
bindsym $mod+Shift+2 move container to workspace $WS2
bindsym $mod+Shift+3 move container to workspace $WS3
bindsym $mod+Shift+4 move container to workspace $WS4
bindsym $mod+Shift+5 move container to workspace $WS5
bindsym $mod+Shift+6 move container to workspace $WS6
bindsym $mod+Shift+7 move container to workspace $WS7
bindsym $mod+Shift+8 move container to workspace $WS8
bindsym $mod+Shift+9 move container to workspace $WS9

bindsym XF86AudioLowerVolume exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -d 5 && pkill -RTMIN+3 i3blocks
bindsym XF86AudioRaiseVolume exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -i 5 && pkill -RTMIN+3 i3blocks
bindsym XF86AudioMute exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -t && pkill -RTMIN+3 i3blocks
bindsym XF86MonBrightnessUp exec --no-startup-id ${pkgs.light}/bin/light +A 10 && pkill -RTMIN+2 i3blocks
bindsym XF86MonBrightnessDown exec --no-startup-id ${pkgs.light}/bin/light -A 10 && pkill -RTMIN+2 i3blocks

mode "  " {
  bindsym Left   resize shrink width 10 px or 10 ppt
  bindsym Down   resize grow height 10 px or 10 ppt
  bindsym Up   resize shrink height 10 px or 10 ppt
  bindsym Right  resize grow width 10 px or 10 ppt
  bindsym Escape mode "default"
  bindsym Space mode "default"
}
bindsym $mod+r mode "  "

#class                  container-border container-bg fg           indicator window-border
client.focused          ${gray.dark}     ${black}     ${white}     ${white}  ${gray.dark}
client.focused_inactive ${black}         ${black}     ${gray.dark} ${white}  ${black}
client.unfocused        ${black}         ${black}     ${gray.dark} ${white}  ${black}
client.urgent           ${red.light}     ${black}     ${white}     ${white}  ${red.light}
client.placeholder      ${black}         ${black}     ${gray.dark} ${white}  ${black}

bar {
  status_command "${pkgs.i3blocks}/bin/i3blocks -c ${pkgs.writeTextFile {
    name = "i3blocks.conf";
    text = import ./i3blocks.nix { inherit pkgs theme; };
  }}"
  position top

  font pango:${uiFont.name},FontAwesome ${toString uiFont.size}
  separator_symbol " // "
  colors {
    separator ${gray.dark}
    background ${black}
    statusline ${gray.dark}

    #                  border   bg       fg
    focused_workspace  ${black} ${black} ${white}
    active_workspace   ${black} ${black} ${gray.light}
    inactive_workspace ${black} ${black} ${gray.dark}
    urgent_workspace   ${black} ${black} ${red.light}
    binding_mode       ${black} ${black} ${red.light}
  }
}
''
