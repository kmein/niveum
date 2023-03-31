{
  pkgs,
  lib,
  niveumPackages,
  config,
  ...
}: {
  # watcher scripts
  url = address:
    pkgs.writers.writeDash "watch-url" ''
      ${pkgs.curl}/bin/curl -sSL ${lib.escapeShellArg address} \
        | ${pkgs.python3Packages.html2text}/bin/html2text --decode-errors=ignore
    '';
  urlSelector = selector: address:
    pkgs.writers.writeDash "watch-url-selector" ''
      ${pkgs.curl}/bin/curl -sSL ${lib.escapeShellArg address} \
        | ${pkgs.htmlq}/bin/htmlq ${lib.escapeShellArg selector} \
        | ${pkgs.python3Packages.html2text}/bin/html2text
    '';
  urlJSON = {jqScript ? "."}: address:
    pkgs.writers.writeDash "watch-url-json" ''
      ${pkgs.curl}/bin/curl -sSL ${lib.escapeShellArg address} | ${pkgs.jq}/bin/jq -f ${pkgs.writeText "script.jq" jqScript}
    '';

  # reporter scripts
  kpaste-irc = {
    target,
    retiolumLink ? false,
    server ? "irc.r",
    messagePrefix ? "change detected: ",
    nick ? ''"$PANOPTIKON_WATCHER"-watcher'',
  }:
    pkgs.writers.writeDash "kpaste-irc-reporter" ''
      ${niveumPackages.kpaste}/bin/kpaste \
        | ${pkgs.gnused}/bin/sed -n "${
        if retiolumLink
        then "2"
        else "3"
      }s/^/${messagePrefix}/p" \
        | ${config.nur.repos.mic92.ircsink}/bin/ircsink \
          --nick ${nick} \
          --server ${server} \
          --target ${target}
    '';
}
