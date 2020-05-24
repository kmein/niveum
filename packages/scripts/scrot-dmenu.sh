#!/bin/sh

# ref https://gitlab.com/dwt1/dotfiles/-/blob/master/.dmenu/dmenu-scrot.sh

APP_NAME="📸 Scrot"
IMG_PATH="$HOME/Downloads/Screenshots"
TIME=3000 #Miliseconds notification should remain visible

cmd=$(printf "fullscreen\nsection\nupload_fullscreen\nupload_section\n" | dmenu -p 'Screenshot')

cd "$IMG_PATH" || exit
case ${cmd%% *} in
  fullscreen)
    scrot -d 1 \
      && notify-send -u low -t $TIME -a "$APP_NAME" 'Screenshot (full screen) saved.'
    ;;

  section)
    scrot -s \
      && notify-send -u low -t $TIME -a "$APP_NAME" 'Screenshot (section) saved.'
    ;;

  upload_fullscreen)
    scrot -d 1 -e "kpaste < \$f" | tail --lines=1 | xclip -selection clipboard -in \
      && notify-send -u low -t $TIME -a "$APP_NAME" "Screenshot (full screen) uploaded: $(xclip -selection clipboard -out)"
    ;;

  upload_section)
    scrot -s -e "kpaste < \$f" | tail --lines=1 | xclip -selection clipboard -in \
      &&  notify-send -u low -t $TIME -a "$APP_NAME" "Screenshot (section) uploaded: $(xclip -selection clipboard -out)"
    ;;
esac
