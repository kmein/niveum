{
  pkgs,
  lib,
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
  niveum.bots.autorenkalender = {
    enable = true;
    time = "07:00";
    mastodon = {
      enable = false;
      language = "de";
      tokenFile = config.age.secrets.mastodon-token-autorenkalender.path;
    };
    telegram = {
      enable = true;
      tokenFile = config.age.secrets.telegram-token-kmein.path;
      chatIds = ["@autorenkalender"];
      parseMode = "Markdown";
    };
    command = "${autorenkalender}/bin/autorenkalender";
  };

  age.secrets = {
    mastodon-token-autorenkalender.file = ../../secrets/mastodon-token-autorenkalender.age;
  };

  niveum.passport.services = [
    {
      title = "Autorenkalender";
      description = "sends <a href=\"https://www.projekt-gutenberg.org/\">Projekt Gutenberg</a>'s anniversary information to Telegram.";
      link = "https://t.me/Autorenkalender";
    }
  ];
}
