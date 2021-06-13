{ lib, fetchurl, writeText, writers, coreutils, dmenu, gnused, libnotify, xclip, xdotool }:
let
  emoji-file = fetchurl {
    url = "https://raw.githubusercontent.com/kmein/unipicker/master/symbols";
    sha256 = "1q6ampv4fhdvd0riz4ihx43gkbyvdab4c38q96ybn0ka9d30vi3g";
  };
  kaomoji-file = writeText "kaomoji.txt" ''
    ¯\(°_o)/¯ dunno lol shrug dlol
    ¯\_(ツ)_/¯ dunno lol shrug dlol
    ( ͡° ͜ʖ ͡°) lenny
    ¯\_( ͡° ͜ʖ ͡°)_/¯ lenny shrug dlol
    ( ﾟдﾟ) aaah sad noo
    ヽ(^o^)丿 hi yay hello
    (^o^: ups hehe
    (^∇^) yay
    ┗(｀皿´)┛ angry argh
    ヾ(^_^) byebye!! bye
    <(^.^<) <(^.^)> (>^.^)> (7^.^)7 (>^.^<) dance
    (-.-)Zzz... sleep
    (∩╹□╹∩) oh noes woot
    (╯°□°）╯ ┻━┻ table flip
    (」゜ロ゜)」 why woot
    (_゜_゜_) gloom I see you
    ༼ ༎ຶ ෴ ༎ຶ༽ sad
    (\/) (°,,,,°) (\/) krebs
    ┳━┳ ヽ(ಠل͜ಠ)ﾉ putting table back
    ┻━┻︵ \(°□°)/ ︵ ┻━┻ flip all dem tablez
    (`･ω･´) bear look
    ᕦ(ຈل͜ຈ)ᕤ strong flex muscle bicep
    ᕦ(ò_óˇ)ᕤ strong flex muscle bicep
    (๑>ᴗ<๑) excite
    (∩ ` -´)⊃━━☆ﾟ.*･｡ﾟ wizard spell magic
    ◕ ◡ ◕ puss in boots big eye
    ≋≋≋≋≋̯̫⌧̯̫(ˆ•̮ ̮•ˆ) nyan cat
    ʕ•ᴥ•ʔ bear
    (ԾɷԾ) adventure time
    (⁀ᗢ⁀) happy yay
    (≧◡≦) happy yay
    ＼(º □ º )/ panic
  '';
in # ref https://github.com/LukeSmithxyz/voidrice/blob/9fe6802122f6e0392c7fe20eefd30437771d7f8e/.local/bin/dmenuunicode
writers.writeDashBin "unicodmenu" ''
  PATH=${lib.makeBinPath [ coreutils dmenu gnused libnotify xclip xdotool ]}
  chosen=$(cat ${emoji-file} ${kaomoji-file} | dmenu -i -l 10 | sed "s/ .*//")

  [ "$chosen" != "" ] || exit

  echo "$chosen" | tr -d '\n' | xclip -selection clipboard

  if [ -n "$1" ]; then
    xdotool key Shift+Insert
  else
    notify-send --app-name="$(basename "$0")" "'$chosen' copied to clipboard." &
  fi
''
