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
        "*\(.word)* '\'''\(.definition | rtrimstr(" "))'\'''\n\nFirst occurrence: \(.firstOccurrence)\nNumber of occurrences: \(.totalOccurrences)"
      '
    '');
    parseMode = "Markdown";
  };
}
