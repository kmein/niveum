{ config, pkgs, lib, ... }:
let
  telebots = let
    telebots-package = pkgs.fetchFromGitHub {
      owner = "kmein";
      repo = "telebots";
      rev = "b4276155114ee96cd3f320e361e52952ea700db6";
      sha256 = "08rp1pcisk4zzhxdlgrlhxa0sbza5qhxa70rjycg4r7fmixkkbz2";
    };
  in {
    reverse =
      pkgs.python3Packages.callPackage "${telebots-package}/telegram-reverse"
      { };
    odyssey =
      pkgs.python3Packages.callPackage "${telebots-package}/telegram-odyssey"
      { };
    betacode =
      pkgs.python3Packages.callPackage "${telebots-package}/telegram-betacode"
      { };
  };
in {
  imports = [
    ./literature-quote.nix
    ./autorenkalender.nix
    ./proverb.nix
    ./nachtischsatan.nix
  ];

  systemd.services.telegram-odyssey = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram bot reciting the Odyssey to you";
    environment.TELEGRAM_ODYSSEY_TOKEN =
      lib.strings.fileContents <system-secrets/telegram/odyssey.token>;
    enable = true;
    script = "${telebots.odyssey}/bin/telegram-odyssey";
    serviceConfig.Restart = "always";
  };

  systemd.services.telegram-reverse = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram bot for reversing things";
    environment.TELEGRAM_REVERSE_TOKEN =
      lib.strings.fileContents <system-secrets/telegram/reverse.token>;
    enable = true;
    script = "${telebots.reverse}/bin/telegram-reverse";
    serviceConfig.Restart = "always";
  };

  systemd.services.telegram-betacode = {
    wantedBy = [ "multi-user.target" ];
    description =
      "Telegram bot for converting Ancient Greek betacode into unicode";
    environment.TELEGRAM_BETACODE_TOKEN =
      lib.strings.fileContents <system-secrets/telegram/betacode.token>;
    enable = true;
    script = "${telebots.betacode}/bin/telegram-betacode";
    serviceConfig.Restart = "always";
  };
}
