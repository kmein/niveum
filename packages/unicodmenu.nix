{
  lib,
  runCommand,
  fetchurl,
  writeText,
  writers,
  coreutils,
  dmenu,
  gnused,
  libnotify,
  xclip,
  xdotool,
  gawk,
}: let
  unicode-file = runCommand "unicode.txt" {} ''
    ${
      writers.writePython3 "generate.py" {flakeIgnore = ["E501" "E722"];} ''
        import csv

        with open("${
          fetchurl {
            url = "https://unicode.org/Public/UCD/latest/ucd/UnicodeData.txt";
            sha256 = "0wva6ygnh3wrzpzy0kcbc32hz1ydx3k2pqc5xkqrfw83cpnrlvl0";
          }
        }", "r") as unicode_data:
            reader = csv.reader(unicode_data, delimiter=";")
            next(reader)  # skip first row containing \0
            for row in reader:
                codepoint = row[0]
                name = row[1]
                alternate_name = row[10]
                try:
                    print(chr(int(codepoint, 16)), codepoint, name, alternate_name, sep="    ")
                except:
                    continue
      ''
    } > $out
  '';
  kaomoji-file = writeText "kaomoji.txt" ''
    ¯\(°_o)/¯    dunno lol shrug dlol
    ¯\_(ツ)_/¯    dunno lol shrug dlol
    ( ͡° ͜ʖ ͡°)    lenny
    ¯\_( ͡° ͜ʖ ͡°)_/¯    lenny shrug dlol
    ( ﾟдﾟ)    aaah sad noo
    ヽ(^o^)丿    hi yay hello
    (^o^:    ups hehe
    (^∇^)    yay
    ┗(｀皿´)┛    angry argh
    ヾ(^_^)    byebye!! bye
    <(^.^<) <(^.^)> (>^.^)> (7^.^)7 (>^.^<)    dance
    (-.-)Zzz...    sleep
    (∩╹□╹∩)    oh noes woot
    (╯°□°）╯ ┻━┻    table flip
    (」゜ロ゜)」    why woot
    (_゜_゜_)    gloom I see you
    ༼ ༎ຶ ෴ ༎ຶ༽    sad
    (\/) (°,,,,°) (\/)    krebs
    ┳━┳ ヽ(ಠل͜ಠ)ﾉ    putting table back
    ┻━┻︵ \(°□°)/ ︵ ┻━┻    flip all dem tablez
    (`･ω･´)    bear look
    ᕦ(ຈل͜ຈ)ᕤ    strong flex muscle bicep
    ᕦ(ò_óˇ)ᕤ    strong flex muscle bicep
    (๑>ᴗ<๑)    excite
    (∩ ` -´)⊃━━☆ﾟ.*･｡ﾟ    wizard spell magic
    ◕ ◡ ◕    puss in boots big eye
    ≋≋≋≋≋̯̫⌧̯̫(ˆ•̮ ̮•ˆ)    nyan cat
    ʕ•ᴥ•ʔ    bear
    (ԾɷԾ)    adventure time
    (⁀ᗢ⁀)    happy yay
    (≧◡≦)    happy yay
    ＼(º □ º )/    panic
    𓂺    penis
    𓂸    penis
  '';
in
  # ref https://github.com/LukeSmithxyz/voidrice/blob/9fe6802122f6e0392c7fe20eefd30437771d7f8e/.local/bin/dmenuunicode
  writers.writeDashBin "unicodmenu" ''
    history_file=$HOME/.cache/unicodmenu
    PATH=${lib.makeBinPath [coreutils dmenu gawk gnused libnotify xclip xdotool]}

    all_characters() {
      tac "$history_file"
      cat ${kaomoji-file} ${unicode-file}
    }

    chosen=$(all_characters | awk '!seen[$0]++' | dmenu -p unicode -i -l 10 | tee --append "$history_file" | sed "s/    .*//")

    [ "$chosen" != "" ] || exit

    echo "$chosen" | tr -d '\n' | xclip -selection clipboard

    if [ -n "$1" ]; then
      xdotool key Shift+Insert
    else
      notify-send --app-name="$(basename "$0")" "'$chosen' copied to clipboard." &
    fi
  ''
