{
  pkgs,
  lib,
  ...
}: let
  scripts = import <niveum/packages/scripts> {inherit pkgs lib;};
  inherit (scripts) literature-quote;
in {
  niveum.telegramBots.quotebot = {
    enable = true;
    time = "08/6:00";
    token = lib.strings.fileContents <system-secrets/telegram/kmein.token>;
    chatIds = ["-1001760262519"];
    command = "${literature-quote}/bin/literature-quote";
    parseMode = "Markdown";
  };

  niveum.passport.services = [
    {
      title = "Literature quote bot";
      description = "sends me and my friends three <a href=\"https://logotheca.xn--kiern-0qa.de/\">logotheca</a> quotes a day.";
    }
  ];
}
