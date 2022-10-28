{
  pkgs,
  lib,
  ...
}: {
  niveum.telegramBots.tlg-wotd = {
    enable = true;
    time = "9:30";
    token = lib.strings.fileContents <system-secrets/telegram/kmein.token>;
    chatIds = ["@tlgwotd"];
    command = toString (pkgs.writers.writeDash "tlg-wotd" ''
      ${pkgs.curl}/bin/curl -sSL http://stephanus.tlg.uci.edu/Iris/Wotd \
      | ${pkgs.recode}/bin/recode html..utf8 \
      | ${pkgs.jq}/bin/jq -r '
        "*\(.word)* '\'''\(.definition | sub("<.*>"; "") | rtrimstr(" "))'\'''\n\nFirst occurrence: \(.firstOccurrence)\nNumber of occurrences: \(.totalOccurrences)"
      '
    '');
    parseMode = "Markdown";
  };

  niveum.passport.services = [
    {
      title = "Thesaurus Linguae Graecae Word of the Day";
      description = "sends <a href=\"https://stephanus.tlg.uci.edu/\">TLG</a>'s word of the day to Telegram.";
      link = "https://t.me/tlgwotd";
    }
  ];
}
