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
    chatIds = ["18980945" "757821027" "455964311"];
    command = "${literature-quote}/bin/literature-quote";
    parseMode = "Markdown";
  };
}
