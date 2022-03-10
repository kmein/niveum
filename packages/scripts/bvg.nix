{
  ruby,
  stdenv,
  bundlerEnv,
  fetchFromGitHub,
}: let
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "bvg";
    rev = "bbfea2e0fdc91a37a34f581c4623704297275b47";
    sha256 = "1iyghksyiy4xkyjw10a7qhy796p88gm9ll6wr7iq55xg98w9mya4";
  };
  env = bundlerEnv {
    name = "bvg-env";
    inherit ruby;
    gemfile = "${src.out}/Gemfile";
    lockfile = "${src.out}/Gemfile.lock";
    gemset = "${src.out}/gemset.nix";
  };
in
  stdenv.mkDerivation {
    name = "bvg";
    buildInputs = [env.wrappedRuby];
    script = "${src.out}/bvg.rb";
    buildCommand = ''
      install -D -m755 $script $out/bin/bvg
      patchShebangs $out/bin/bvg
    '';
  }
