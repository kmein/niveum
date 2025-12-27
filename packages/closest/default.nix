{
  writers,
  fetchurl,
  haskellPackages,
}:
writers.writeDashBin "closest" ''
  ${
    writers.writeHaskellBin "closest" {
      libraries = with haskellPackages; [
        parallel
        optparse-applicative
        edit-distance
      ];
      ghcArgs = [
        "-O3"
        "-threaded"
      ];
    } (builtins.readFile ./distance.hs)
  }/bin/closest +RTS -N4 -RTS --dictionary ${
    fetchurl {
      url = "https://gist.github.com/MarvinJWendt/2f4f4154b8ae218600eb091a5706b5f4/raw/36b70dd6be330aa61cd4d4cdfda6234dcb0b8784/wordlist-german.txt";
      sha256 = "0vr4lmlckgvj4s8sk502sknq9pf3297rvasj5sqqm05zzbdgpppj";
    }
  } "$@"
''
