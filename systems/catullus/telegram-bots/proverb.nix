{ pkgs, lib, ... }:
let
  proverb-bot-package = pkgs.fetchFromGitHub {
    owner = "kmein";
    repo = "proverb-pro";
    rev = "f4201c5419354377a26b7f7873368683efbea417";
    sha256 = "1ixffmxy3sxy2if7fd44ps451rds14hnz4d0x9nkh8lzshqk6v4y";
  };
  telegram-proverb = pkgs.python3Packages.callPackage proverb-bot-package { };
in {
  systemd.services.telegram-proverb = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram bot for generating inspiring but useless proverbs";
    environment.TELEGRAM_PROVERB_TOKEN =
      lib.strings.fileContents <secrets/telegram/proverb.token>;
    enable = true;
    script = "${telegram-proverb}/bin/proverb_bot.py";
    serviceConfig.Restart = "always";
  };
}
