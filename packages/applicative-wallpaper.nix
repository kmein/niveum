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
    # the SVG rasterizes to opaque black-on-white without an alpha channel,
    # so recolor by remapping the grayscale range instead of -opaque:
    # black -> foreground, white -> background (antialiasing interpolates)
    magick \
      -density 300 \
      -background white \
      "${logoSvg}" \
      -flatten \
      -resize 800x800 \
      -colorspace gray \
      +level-colors "${foregroundColor}","${backgroundColor}" \
      -gravity center \
      -background "${backgroundColor}" \
      -extent ${toString width}x${toString height} \
      $out
  ''
