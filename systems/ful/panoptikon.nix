{
  config,
  pkgs,
  lib,
  ...
}: let
  kpaste = pkgs.writers.writeDash "kpaste" ''
    ${pkgs.curl}/bin/curl -sS -sS http://p.r --data-binary @"''${1:--}" | ${pkgs.gnused}/bin/sed '$ {p;s|http://p.r|https://p.krebsco.de|}'
  '';

  url = address:
    pkgs.writers.writeDash "watch-url" ''
      ${pkgs.w3m}/bin/w3m -dump ${lib.escapeShellArg address}
    '';

  urlJSON = address:
    pkgs.writers.writeDash "watch-url-json" ''
      ${pkgs.curl}/bin/curl -sSL ${lib.escapeShellArg address} | ${pkgs.jq}/bin/jq
    '';

  reporters.irc-xxx = pkgs.writers.writeDash "irc-xxx" ''
    ${kpaste} \
    | ${pkgs.gnused}/bin/sed -n '2s/^/change detected: /p' \
    | ${config.nur.repos.mic92.ircsink}/bin/ircsink \
      --nick "$PANOPTIKON_WATCHER"-watcher \
      --server irc.r \
      --target '#xxx'
  '';
in {
  services.panoptikon = {
    enable = true;
    watchers."github-meta" = {
      script = urlJSON "https://api.github.com/meta";
      reporters = [reporters.irc-xxx];
    };
    watchers.spiegel = {
      script = url "https://www.spiegel.de/";
      reporters = [];
    };
  };
}
