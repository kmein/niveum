{
  writers,
  mpv,
}: let
  arabicStories = /home/kfm/cloud/syncthing/music/Arabic/Stories;
  levantineTextbook = /home/kfm/cloud/syncthing/music/Arabic/Damaszenisch;
in
  writers.writeDashBin "polyglot" ''
    languages='persian
    arabic
    coptic
    sanskrit
    levantine
    hebrew'

    kurdish="https://www.youtube.com/channel/UCvutKJerMREoQtzXiQaDNBQ
    https://www.youtube.com/channel/UCdqSEXLhnsltwN4IESMInDA"

    persian="https://www.youtube.com/playlist?list=PL4aDVDOklYH5MnXNjCeRalFuRZ46I_p8Q
    https://www.youtube.com/playlist?list=PL4aDVDOklYH404fg2zZoUQR6YcQp4UUrW
    https://www.youtube.com/playlist?list=PL4aDVDOklYH4FhJZ4cX14HuUVsDJKUifr
    https://www.youtube.com/playlist?list=PL4aDVDOklYH6i_Od0zD4ZFNqmZVBOTo4z
    https://www.youtube.com/playlist?list=PL4aDVDOklYH5NuhQvc52KCPO0X64FU_bd
    https://www.youtube.com/playlist?list=PL4aDVDOklYH6MFQfuQ5VIJmwuKfR1kgUR
    https://www.youtube.com/playlist?list=PL4aDVDOklYH48_uuVl-AAPbkemzZFvnHH
    https://www.youtube.com/playlist?list=PL4aDVDOklYH61WM-WmzjZyTFAE7AILb7j
    https://www.youtube.com/channel/UCGY0LHNDQjt3GQrrc3r3Atw
    https://www.youtube.com/channel/UC4jgHye1-kjDlY-2StrtVtA
    https://www.youtube.com/channel/UCf67DKdLhpFW-7c7FZre2Ww
    https://www.youtube.com/channel/UCLOGyLCPJL99gNriGAhwl7g
    https://www.youtube.com/channel/UCxV5ZfGJjJhrzy_9am-S4QQ
    https://www.youtube.com/channel/UCBSF89JJieetWjJTGZhGKJA
    https://www.youtube.com/channel/UCFGB29XZkEGS1Vw7WplBqIg
    https://www.youtube.com/channel/UChiyq4qjnAWMNhwPu2KL4yg
    https://www.youtube.com/channel/UCULxPJn3NjsaXt4Nc-sIZrw
    https://www.youtube.com/channel/UCYRyoX3ru_BfMiXVCGgRS6w
    https://www.youtube.com/channel/UCbCvjr0v_-8LmZh9431N84w"

    hebrew="https://www.youtube.com/playlist?list=PLXU4ackZsIPp2G8XjfpsYso2p3IHyl-bJ
    https://www.youtube.com/playlist?list=PLXU4ackZsIPqLOWeWZc1frv3VCAohi90p
    https://www.youtube.com/playlist?list=PLXU4ackZsIPpWWdWvtM3UtZHFKFhmI2mj
    https://www.youtube.com/playlist?list=PLXU4ackZsIPoz05eaLCHsCv4Wrk4n1g34
    https://www.youtube.com/channel/UCrMYJpbMhhQZhXi4ui3FKEw
    https://www.youtube.com/playlist?list=PLXU4ackZsIPombqx98SIPanShjSUSlBW-
    https://www.youtube.com/playlist?list=PLXU4ackZsIPrdSFUjNdw3eGK-qbtgqt-6
    https://www.youtube.com/playlist?list=PLXU4ackZsIPrVHjB4P9QrYzJvBKD3MLuA
    https://www.youtube.com/playlist?list=PLXU4ackZsIPp_7fb7TMyOaaX_ORI6vH29
    https://www.youtube.com/channel/UCkKmeTinUEU27syZPKrzWQQ
    https://www.youtube.com/channel/UC2gy2POCchS7JM_UCsZx5dw
    https://www.youtube.com/channel/UCb2bkA-kSUz4Mj5YJv0zpJQ"

    arabic="https://www.youtube.com/channel/UCbqqV0gO5QbV9iGITdxp-cw
    https://www.youtube.com/channel/UCxNwNoGEhHg7lGOhthG4r6A
    https://www.youtube.com/channel/UCmYYUdR85LRVB5yT1Y7DjFA
    https://www.youtube.com/channel/UCIgFDroRoDYnxBlOGmwJ78A
    https://www.youtube.com/channel/UCn5ASYdp7CzbFH2qtqjIJ9w
    https://www.youtube.com/channel/UCD8N8HjsCkCmykfPnVleVRg
    https://www.youtube.com/channel/UCEmWUZanVYXEzZXYDHzD-iA
    https://www.youtube.com/channel/UC9rnrMdYzfqdjiuNXV7q8oQ
    https://live-hls-audio-web-aja.getaj.net/VOICE-AJA/01.m3u8
    http://asima.out.airtime.pro:8000/asima_a
    http://edge.mixlr.com/channel/qtgru
    http://ninarfm.grtvstream.com:8896/stream
    http://andromeda.shoutca.st:8189/stream
    http://www.dreamsiteradiocp4.com:8014/stream
    http://n02.radiojar.com/sxfbks1vfy8uv.mp3
    http://stream-025.zeno.fm/5y95pu36sm0uv
    ${arabicStories}"

    levantine_general="https://www.youtube.com/channel/UCe6YxTdT2zsbhG8ThAEssLw
    https://www.youtube.com/channel/UC8IsrQ3Fvg1X2QboSRIMBHA
    https://www.youtube.com/channel/UCo65IZihlwP204bleDDuAyA
    https://www.youtube.com/channel/UCDXBymJu72YX2LzKVlZyZaA
    https://www.youtube.com/channel/UCpovzufzZSP3kCYm16B5Hyw
    https://www.youtube.com/channel/UCKkKlH7eJFBWhofiP5kQEFQ
    https://www.youtube.com/channel/UC-YYp3mws0sa9vI3VUCWqhw
    https://www.youtube.com/channel/UCpa9WD4btPSyn0h-DZSATGw
    https://www.youtube.com/channel/UCb7oMrqwZnr3ZCayqnkkb8w
    https://www.youtube.com/channel/UCuLNZirpkm2HYxq-tTiXnOA
    https://www.youtube.com/channel/UCSGBoIBGUxUmpTYJfYZXs-A
    https://www.youtube.com/channel/UCPINCItSdAc7SBXxi6AcWpw
    https://www.youtube.com/channel/UCbZvzUBn04a_a95HFMX6eTA
    https://www.youtube.com/channel/UChJs6Kqju9BmN5FMHhBfRSA
    https://www.youtube.com/channel/UCyC7OV3gEQkguVwl9qDGuTQ
    https://www.youtube.com/channel/UCwv1qu4iX6Bm6X-uSSB2eBQ
    ${levantineTextbook}"

    sanskrit="https://www.youtube.com/channel/UCTnCQNG_1WIlunxbp1SAOvw
    https://stream-23.zeno.fm/m08mkwsyw8quv
    https://www.youtube.com/channel/UCqFg6QnwgtVHo1iFgpxrx-A"

    language="$(echo "$languages" | shuf -n1)"
    case "$language" in
      arabic)
        ${mpv}/bin/mpv --shuffle $arabic;;
      persian)
        ${mpv}/bin/mpv --shuffle $persian;;
      coptic)
        ;;
      sanskrit)
        ${mpv}/bin/mpv --shuffle $sanskrit;;
      levantine)
        ${mpv}/bin/mpv --shuffle $levantine;;
      hebrew)
        ${mpv}/bin/mpv --shuffle $hebrew;;
    esac
  ''
