{
  writers,
  imagemagick,
}:
writers.writeDashBin "image-convert-favicon" ''
  ${imagemagick}/bin/convert "$1" -define icon:auto-resize=64,48,32,16 "''${2-favicon.ico}"
''
