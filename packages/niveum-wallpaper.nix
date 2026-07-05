{
  runCommand,
  imagemagick,
  colors,
}:
let
  c = colors.withHashtag;
in
runCommand "niveum-wallpaper.png"
  {
    nativeBuildInputs = [ imagemagick ];
  }
  ''
    # scheme background with three diagonal accent stripes in the bottom-right corner
    convert -size 1920x1080 xc:"${c.base00}" \
      -draw "fill ${c.base0A} polygon 1520,1080 1620,1080 1920,780 1920,680" \
      -draw "fill ${c.base09} polygon 1620,1080 1720,1080 1920,880 1920,780" \
      -draw "fill ${c.base08} polygon 1720,1080 1820,1080 1920,980 1920,880" \
      $out
  ''
