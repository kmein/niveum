{
  runCommand,
  fetchurl,
  imagemagick,
  colors,
}:
let
  backgroundColor = colors.withHashtag.base06;
  foregroundColor = colors.withHashtag.base01;
  width = 1920;
  height = 1080;

  svgUrl = "https://applicative.systems/_astro/logo-full.D8zRvqBZ.svg";
  logoSvg = fetchurl {
    url = svgUrl;
    hash = "sha256-qXDIEZsAPn4eUJ3kb5U6L3PMUCtWGYqhqyIaBt7FntE=";
  };
in
runCommand "applicative-wallpaper.png"
  {
    nativeBuildInputs = [ imagemagick ];
  }
  ''
    # 1. We use -background to set the canvas color
    # 2. We use -fuzz and -opaque to replace the logo's internal colors
    # 3. We use -gravity and -extent to center it on a wallpaper-sized canvas

    convert \
      -background none \
      -density 300 \
      "${logoSvg}" \
      -fuzz 100% -fill "${foregroundColor}" -opaque black \
      -resize 800x800 \
      -gravity center \
      -background "${backgroundColor}" \
      -extent ${toString width}x${toString height} \
      $out
  ''
