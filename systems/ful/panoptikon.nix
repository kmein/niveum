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

  urlSelector = selector: address:
    pkgs.writers.writeDash "watch-url-selector" ''
      ${pkgs.curl}/bin/curl -sSL ${lib.escapeShellArg address} \
        | ${pkgs.htmlq}/bin/htmlq ${lib.escapeShellArg selector} \
        | ${pkgs.python3Packages.html2text}/bin/html2text
    '';

  reporters.irc-xxx = pkgs.writers.writeDash "irc-xxx" ''
    ${kpaste} \
    | ${pkgs.gnused}/bin/sed -n '2s/^/change detected: /p' \
    | ${config.nur.repos.mic92.ircsink}/bin/ircsink \
      --nick "$PANOPTIKON_WATCHER"-watcher \
      --server irc.r \
      --target '#xxx'
  '';

  reporters.irc-kmein = pkgs.writers.writeDash "irc-xxx" ''
    ${kpaste} \
    | ${pkgs.gnused}/bin/sed -n "3s/^/$PANOPTIKON_WATCHER: /p" \
    | ${config.nur.repos.mic92.ircsink}/bin/ircsink \
      --nick panoptikon-kmein \
      --server irc.r \
      --target 'kmein'
  '';
in {
  services.panoptikon = {
    enable = true;
    watchers = {
      "github-meta" = {
        script = urlJSON "https://api.github.com/meta";
        reporters = [reporters.irc-xxx];
      };
      lammla = {
        script = url "http://lammla.info/index.php?reihe=30";
        reporters = [reporters.irc-kmein];
      };
      kratylos = {
        script = url "https://kratylos.reichert-online.org/current_issue/KRATYLOS";
        reporters = [reporters.irc-kmein];
      };
      zeno-free = {
        script = urlSelector ".zenoCOMain" "http://www.zeno.org/Lesesaal/M/E-Books";
        reporters = [reporters.irc-kmein];
      };
      carolinawelslau = {
        script = urlSelector "#main" "https://carolinawelslau.de/";
        reporters = [reporters.irc-kmein];
      };
      lisalittmann = {
        script = urlSelector "#site-content" "https://lisalittmann.de/";
        reporters = [reporters.irc-kmein];
      };
      lisalittmann-archive = {
        script = urlSelector "#site-content" "https://lisalittmann.de/archive/";
        reporters = [reporters.irc-kmein];
      };
      lisalittmann-projects = {
        script = urlSelector "#site-content" "https://lisalittmann.de/projects/";
        reporters = [reporters.irc-kmein];
      };
      tatort = {
        script = urlSelector ".linklist" "https://www.daserste.de/unterhaltung/krimi/tatort/sendung/index.html";
        reporters = [reporters.irc-kmein];
      };
      warpgrid-idiomarium = {
        script = urlSelector "#site-content" "https://warpgrid.de/idiomarium/";
        reporters = [reporters.irc-kmein];
      };
      warpgrid-futurism = {
        script = urlSelector "#site-content" "https://warpgrid.de/futurism/";
        reporters = [reporters.irc-kmein];
      };
      warpgrid-imagiary = {
        script = urlSelector "#site-content" "https://warpgrid.de/imagiary/";
        reporters = [reporters.irc-kmein];
      };
      warpgrid-alchemy = {
        script = urlSelector "#site-content" "https://warpgrid.de/alchemy/";
        reporters = [reporters.irc-kmein];
      };
      indogermanische-forschungen = {
        script = urlSelector "#latestIssue" "https://www.degruyter.com/journal/key/INDO/html";
        reporters = [reporters.irc-kmein];
      };
      ig-neuigkeiten = {
        script = urlSelector "[itemprop=articleBody]" "https://www.indogermanistik.org/aktuelles/neuigkeiten.html";
        reporters = [reporters.irc-kmein];
      };
      ig-tagungen = {
        script = urlSelector "[itemprop=articleBody]" "https://www.indogermanistik.org/tagungen/tagungen-der-ig.html";
        reporters = [reporters.irc-kmein];
      };
      fxght-or-flxght = {
        script = pkgs.writers.writeDash "watch-url-json" ''
          ${pkgs.curl}/bin/curl -sSL 'https://api.tellonym.me/profiles/name/fxght.or.flxght?limit=20' \
          | ${pkgs.jq}/bin/jq '.answers | map(
            select(.type == "answer")
            | {
               question: .tell,
               answer: .answer,
               date: .createdAt,
               media: .media | map(.url)
            }
          )'
        '';
        reporters = [reporters.irc-kmein];
      };
    };
  };
}
