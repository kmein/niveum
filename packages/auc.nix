{
  stdenv,
  fetchFromGitHub,
  lib,
  pandoc,
}: let
  owner = "kamalist";
in
  stdenv.mkDerivation rec {
    pname = "auc";
    version = "2019-04-02";

    src = fetchFromGitHub {
      inherit owner;
      repo = "AUC";
      rev = "66d1cd57472442b4bf3c1ed12ca5cadd57d076b3";
      sha256 = "0gb4asmlfr19h42f3c5wg9c9i3014if3ymrqan6w9klgzgfyh2la";
    };

    installPhase = ''
      mkdir -p $out/{bin,man/man1}
      install auc $out/bin
      ${pandoc}/bin/pandoc -V title=${lib.escapeShellArg pname} -V section=1 $src/README.md -s -t man -o $out/man/man1/auc.1
    '';

    meta = with lib; {
      description = "Command-line Roman calendar";
      longDescription = ''
        AUC (Ab Urbe condita) is a command-line Roman calendar tool. Currently it shows the specified date in the format of the Ancient Romans.
      '';
      license = licenses.mit;
      maintainers = [maintainers.kmein];
      platforms = platforms.all;
    };
  }
