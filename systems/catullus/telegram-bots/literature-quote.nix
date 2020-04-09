{ pkgs, lib, ... }:
let
  literature-quote = pkgs.callPackage <niveum/packages/scripts/literature-quote.nix> {};
in
{
  niveum.telegramBots.quotebot = {
    enable = true;
    time = "08/6:00";
    token = lib.strings.fileContents <secrets/telegram/kmein.token>;
    chatIds = [ "18980945" "757821027" "455964311" ];
    command = "${literature-quote}/bin/literature-quote";
    parseMode = "Markdown";
  };
}
