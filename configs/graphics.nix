{ pkgs, lib, config, ... }:
let
  spotify_info = pkgs.writeBash "spotify.info" ''
    if $(pgrep spotify); then
      STATUS=$(${pkgs.dbus}/bin/dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:'org.mpris.MediaPlayer2.Player' string:'PlaybackStatus'|egrep -A 1 "string"|cut -b 26-|cut -d '"' -f 1|egrep -v ^$)

      if [[ "$STATUS" == 'Playing' ]]; then
        printf '\uf1bc  '
        printf '\uf04b'
      elif [[ "$STATUS" == 'Paused' ]]; then
        printf '\uf1bc  '
        printf '\uf04c'
      elif [[ "$STATUS" == 'Stopped' ]]; then
        printf '\uf1bc  '
        printf '\uf04d'
      else
        exit 1
      fi

      printf '  '

      METADATA=$(${pkgs.dbus}/bin/dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:'org.mpris.MediaPlayer2.Player' string:'Metadata')
      ARTIST=$(echo "$METADATA" | egrep -A 2 "artist" | egrep -v "artist" | egrep -v "array" | cut -b 27- | cut -d '"' -f 1 | egrep -v ^$)
      TITLE=$(echo "$METADATA" | egrep -A 1 "title" | egrep -v "title" | cut -b 44- | cut -d '"' -f 1 | egrep -v ^$)

      printf "%s \u2237 %s" "$ARTIST" "$TITLE"
    fi
  '';
  battery_info = pkgs.writeBash "battery.info" ''
    BAT_DIR="/sys/class/power_supply/$BLOCK_INSTANCE/"
    if [ -d "$BAT_DIR" ]; then
      cd "$BAT_DIR"
    else
      exit 1
    fi

    if [ -e charge_now ] && [ -e charge_full ]; then
      FULL_CHARGE=charge_full
      CURR_CHARGE=charge_now
    elif [ -e energy_now ] && [ -e energy_full ]; then
      FULL_CHARGE=energy_full
      CURR_CHARGE=energy_now
    else
      ls >&2
      exit 1
    fi

    status=$(cat status)
    charge_f=$((100 * $(cat $CURR_CHARGE) / $(cat $FULL_CHARGE)))

    if [[ "$charge_f" -lt 20 ]]; then
      printf '\uf244'
    elif [[ "$charge_f" -lt 40 ]]; then
      printf '\uf243'
    elif [[ "$charge_f" -lt 60 ]]; then
      printf '\uf242'
    elif [[ "$charge_f" -lt 80 ]]; then
      printf '\uf241'
    else
      printf '\uf240'
    fi

    printf '  '

    if [[ "$status" == 'Charging' ]]; then
      printf '\uf106'
    elif [[ "$status" == 'Discharging' ]]; then
      printf '\uf107'
    elif [[ "$status" == 'Full' ]]; then
      printf '\uf0e7'
    else
      printf '[%s]' "$status"
    fi

    printf '  '

    if [[ "$status" != 'Full' ]]; then
      rate_raw=$(($(cat voltage_now) * $(cat power_now)))
      rate=$(bc <<< "scale=1; $rate_raw / 10^12")
      printf '%s\u2009W, ' "$rate"
    fi

    charge_d=$((100 * $(cat $CURR_CHARGE) / $(cat $FULL_CHARGE)))
    printf '%s%%\n' "$charge_d"

    if [[ "$status" == 'Discharging' ]]; then
      if [[ "$charge_d" -lt 10 ]]; then
        printf '\n#E41C28'
      elif [[ "$charge_d" -lt 20 ]]; then
        printf '\n#EEBF13'
      fi
    fi
  '';
  volume_info = pkgs.writeBash "volume.info" ''
    if [[ "$BLOCK_BUTTON" == 1 ]]; then
      ${pkgs.pamixer}/bin/pamixer -i 5
    elif [[ "$BLOCK_BUTTON" == 3 ]]; then
      ${pkgs.pamixer}/bin/pamixer -d 5
    elif [[ "$BLOCK_BUTTON" == 2 ]]; then
      ${pkgs.pamixer}/bin/pamixer -t
    fi

    if $(${pkgs.pamixer}/bin/pamixer --get-mute); then
      printf '\uf026  0%%\n\n#EEBF13'
    else
      volume=$(${pkgs.pamixer}/bin/pamixer --get-volume)
      printf '\uf028  %s%%' "$volume"
    fi
  '';
  fancyDate = pkgs.writeC "fancy_date.c" {} ''
    #include <stdio.h>
    #include <time.h>
    #include <wchar.h>

    int main(void) {
        time_t now = time(NULL);
        struct tm *today = localtime(&now);
        wchar_t roman_month = 0x2160 + today->tm_mon;
        wprintf(L"%d\u2009%lc\u2009%d [%d]\n", today->tm_mday, roman_month, 1900 + today->tm_year, today->tm_yday/7 + 1);
        return 0;
    }
  '';
  i3blocks_conf = with import ../theme.nix; ''
    markup=pango
    align=center
    color=${white}

    [spotify]
    command=${spotify_info}
    interval=2

    [separator]

    [volume]
    command=${volume_info}
    min_width= 100%
    interval=once
    signal=3

    [separator]

    [cpu_usage]
    command=cut -d' ' -f 1-3 < /proc/loadavg
    label=
    interval=2

    [separator]

    [ram_usage]
    command=free -h | grep "Mem" | awk '{print $3}'
    label=
    interval=2
    align=center

    [separator]

    [battery]
    command=${battery_info}
    interval=10
    instance=BAT1

    [separator]

    [date]
    command=${fancyDate}
    interval=1
    label=

    [separator]

    [time]
    command=date +'%H:%M'
    interval=1
    label=

    [separator]
    command=${pkgs.xkblayout-state}/bin/xkblayout-state print %s
    label=
    interval=2

    [separator]
  '';
  i3_conf = with import ../theme.nix; ''
    set $mod Mod4

    font pango:${uiFont.name} ${toString uiFont.size}
    floating_modifier $mod

    hide_edge_borders both
    new_window pixel 1
    new_float  pixel 1

    bindsym $mod+Return exec ${config.defaultApplications.terminal}
    bindsym $mod+y exec ${config.defaultApplications.browser}
    bindsym $mod+t exec ${config.defaultApplications.fileManager}
    bindsym $mod+Shift+w exec ${pkgs.xautolock}/bin/xautolock -locknow
    bindsym $mod+d exec ${pkgs.rofi}/bin/rofi -display-run — -show run
    bindsym $mod+a exec ${pkgs.rofi}/bin/rofi -display-window — -show window

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

    ${with lib;
      strings.concatMapStringsSep "\n"
        (i: let n = toString i; ws = "$workspace" + n; in
          ''set ${ws} ${n}
          bindsym $mod+${n} workspace ${ws}
          bindsym $mod+Shift+${n} move container to workspace ${ws}'')
        (lists.range 0 9)
    }

    bindsym XF86AudioLowerVolume exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -d 5 && pkill -RTMIN+3 i3blocks
    bindsym XF86AudioRaiseVolume exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -i 5 && pkill -RTMIN+3 i3blocks
    bindsym XF86AudioMute exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -t && pkill -RTMIN+3 i3blocks

    mode "  " {
      bindsym Left   resize shrink width 10 px or 10 ppt
      bindsym Down   resize grow height 10 px or 10 ppt
      bindsym Up   resize shrink height 10 px or 10 ppt
      bindsym Right  resize grow width 10 px or 10 ppt
      bindsym Escape mode "default"
      bindsym Space mode "default"
    }
    bindsym $mod+r mode "  "

    #class                  container-border container-bg fg             indicator window-border
    client.focused          ${gray.dark}     ${black}     ${white}       ${white}  ${gray.medium}
    client.focused_inactive ${black}         ${black}     ${gray.medium} ${white}  ${black}
    client.unfocused        ${black}         ${black}     ${gray.medium} ${white}  ${black}
    client.urgent           ${red.light}     ${black}     ${white}       ${white}  ${red.light}
    client.placeholder      ${black}         ${black}     ${gray.medium} ${white}  ${black}

    bar {
      status_command "${pkgs.i3blocks}/bin/i3blocks -c ${pkgs.writeText "i3blocks.conf" i3blocks_conf}"
      position top

      font pango:${uiFont.name},FontAwesome ${toString uiFont.size}
      separator_symbol " // "
      colors {
        separator ${gray.medium}
        background ${black}
        statusline ${gray.medium}

        #                  border   bg       fg
        focused_workspace  ${black} ${black} ${white}
        active_workspace   ${black} ${black} ${gray.medium}
        inactive_workspace ${black} ${black} ${gray.medium}
        urgent_workspace   ${black} ${black} ${red.light}
        binding_mode       ${black} ${black} ${red.light}
      }
    }
'';
in {
  services.xserver = with import ../helpers.nix; with import ../theme.nix; {
    enable = true;
    layout = commaSep [ "de" "gr" "ru" ];
    xkbVariant = commaSep [ "T3" "polytonic" "phonetic_winkeys" ];
    xkbOptions = commaSep [ "terminate:ctrl_alt_bksp" "grp:alt_space_toggle" ];
    libinput.enable = true;
    xautolock = {
      enable = true;
      killer = "${pkgs.systemd}/bin/systemctl suspend";
      locker = config.defaultApplications.locker;
      nowlocker = config.defaultApplications.locker;
      enableNotifier = true;
      notifier = ''${pkgs.libnotify}/bin/notify-send -u normal -a xautolock "Locking soon" "The screen will lock in 10 seconds."'';
    };
    displayManager.auto = { enable = true; user = "kfm"; };
    displayManager.sessionCommands = "${pkgs.dropbox-cli}/bin/dropbox start";
    desktopManager.xterm.enable = false;
    desktopManager.wallpaper.mode = "fill";
    windowManager.default = "i3";
    windowManager.i3 = {
      enable = true;
      configFile = pkgs.writeText "i3.conf" i3_conf;
      extraPackages = [];
    };
    xrandrHeads = {
      homeros = [ "LVDS1" { output = "HDMI1"; primary = true; } ];
      scardanelli = [ "eDP1" ];
    }.${config.networking.hostName};
  };

  i18n = {
    defaultLocale = "en_GB.UTF-8";
    consoleUseXkbConfig = true;
    consoleColors = with import ../theme.nix; map (c: lib.strings.removePrefix "#" c) colorPalette;
  };

  services.compton = {
    enable = true;
    shadow = true;
    menuOpacity = "0.9";
    shadowOpacity = "0.3";
  };

  services.redshift = {
    enable = true;
    latitude = "52";
    longitude = "13";
  };

  services.illum.enable = true;

  services.unclutter = {
    enable = true;
    timeout = 10;
  };
}
