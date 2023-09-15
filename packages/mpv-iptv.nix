{
  mpv,
  writers,
}:
writers.writeDashBin "iptv" ''
  set -efu
  ${mpv}/bin/mpv \
    --audio-display=no --audio-channels=stereo \
    --audio-samplerate=48000 --audio-format=s16 \
    --ao-pcm-file=/run/snapserver/snapfifo --ao=pcm \
    --audio-delay=-1 \
    --playlist=https://iptv-org.github.io/iptv/index.nsfw.m3u \
    --idle=yes \
    --input-ipc-server=/tmp/mpv.ipc \
    "$@"
''
