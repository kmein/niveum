{
  pkgs,
  inputs,
  config,
  ...
}: let
  autorenkalender-package = pkgs.fetchFromGitHub {
    owner = "kmein";
    repo = "autorenkalender";
    rev = "cf49a7b057301332d980eb47042a626add93db66";
    sha256 = "1pa7sjg33vdnjianrqldv445jdzzv3mn231ljk1j58hs0cd505gs";
  };
  autorenkalender =
    pkgs.python3Packages.callPackage autorenkalender-package {};
in {
  niveum.telegramBots.autorenkalender = {
    enable = true;
    time = "07:00";
    tokenFile = config.age.secrets.telegram-token-kmein.path;
    chatIds = ["@autorenkalender"];
    parseMode = "Markdown";
    command = "${autorenkalender}/bin/autorenkalender";
  };

  age.secrets.telegram-token-kmein.file = inputs.secrets + "/telegram-token-kmein.age";

  niveum.passport.services = [
    {
      title = "Autorenkalender";
      description = "sends <a href=\"https://www.projekt-gutenberg.org/\">Projekt Gutenberg</a>'s anniversary information to Telegram.";
      link = "https://t.me/Autorenkalender";
    }
  ];
}
