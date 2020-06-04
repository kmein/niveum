#!/bin/sh
options='Pastebin
Shorten
[Replace p.r]'

r_to_krebscode() {
  sed 's/\<r\>/krebsco.de/'
}

modify_clipboard() {
  case $(echo "$options" | dmenu -i -p 'clipboard') in
    'Pastebin')
      curl -fSs -F 'f:1=<-' ix.io
      # curl -fSs http://p.r --data-binary @- | tail --lines=1 | r_to_krebscode
    ;;
    'Shorten')
      # curl -fSs "http://tinyurl.com/api-create.php?url=$(cat)"
      curl -fSs "https://0x0.st" -F "shorten=$(cat)"
      # curl -fSs http://go.r -F "uri=$(cat)"
    ;;
    'Replace p.r') r_to_krebscode ;;
    *) cat;;
  esac
}

xclip -selection clipboard -out \
  | modify_clipboard \
  | tr -d '\r\n' \
  | xclip -selection clipboard -in
