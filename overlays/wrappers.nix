# wrapped from upstream
{ voidrice }:
final: prev: {
  wrapScript =
    {
      packages ? [ ],
      name,
      script,
    }:
    prev.writers.writeDashBin name ''PATH=$PATH:${
      prev.lib.makeBinPath (
        packages
        ++ [
          final.findutils
          final.coreutils
          final.gnused
          final.gnugrep
        ]
      )
    } ${script} "$@"'';
  tag = final.wrapScript {
    script = voidrice.outPath + "/.local/bin/tag";
    name = "tag";
    packages = [ final.ffmpeg ];
  };
  booksplit = final.wrapScript {
    script = voidrice.outPath + "/.local/bin/booksplit";
    name = "booksplit";
    packages = [
      final.ffmpeg
      final.glibc.bin
    ];
  };
  dmenu = prev.writers.writeDashBin "dmenu" ''exec ${final.rofi}/bin/rofi -dmenu "$@"'';
}
