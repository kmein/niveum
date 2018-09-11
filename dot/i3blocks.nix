{ pkgs, theme }:
let
  spotify_info = pkgs.writeScript "spotify.info" ''
    #!/bin/bash

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
  '';
  battery_info = pkgs.writeScript "battery.info" ''
    #!/usr/bin/env bash
    cd "/sys/class/power_supply/$BLOCK_INSTANCE/"

    status=$(cat status)
    charge_f=$((100 * $(cat charge_now) / $(cat charge_full)))

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

    charge_d=$((100 * $(cat charge_now) / $(cat charge_full)))
    printf '%s%%\n' "$charge_d"

    if [[ "$status" == 'Discharging' ]]; then
      if [[ "$charge_d" -lt 10 ]]; then
        printf '\n#E41C28'
      elif [[ "$charge_d" -lt 20 ]]; then
        printf '\n#EEBF13'
      fi
    fi
  '';
  volume_info = pkgs.writeScript "volume.info" ''
    #!/usr/bin/env bash
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
  fancyDate = pkgs.writeScript "fancy_date.py" ''
    #!/usr/bin/env python3
    from datetime import datetime
    now = datetime.now()
    print(now.strftime("%d\u2009{}\u2009%Y ⟨%V⟩").format(chr(0x2160 + (now.month - 1))))
  ''; in
with theme;
''
markup=pango
align=center
color=${white}

[spotify]
command=${spotify_info}
interval=1

[separator]
command=${pkgs.xkblayout-state}/bin/xkblayout-state print %s
label=
interval=2

[separator]

[volume]
command=${volume_info}
min_width= 100%
interval=once
signal=3

[separator]

[brightness]
command=printf "%.1f%%" $(${pkgs.xorg.xbacklight}/bin/xbacklight)
label=
min_width= 100%
signal=2
interval=once

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
#echo "$(cat /sys/class/power_supply/BAT1/capacity)% ($(cat /sys/class/power_supply/BAT1/status))"
interval=10
instance=BAT1

[separator]

[date]
command=${fancyDate}
interval=30
label=

[separator]

[time]
command=date +'%H:%M'
interval=30
label=

[separator]
''
