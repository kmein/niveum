{
  config,
  pkgs,
  lib,
  ...
}:
let
  # from pr-notifier's throwaway account: the alerts' own mailbox password must not reach ful
  credentials = config.age.secrets.pr-notifier-smtp.path;
  recipient = "2210@cock.li";
  # panoptikon's own mail reporter needs a local MTA, which ful lacks
  mail = pkgs.writers.writeDash "mail-reporter" ''
    set -efu
    user=$(${pkgs.coreutils}/bin/cut -d: -f1 ${credentials})
    {
      printf 'From: %s\nTo: %s\nSubject: %s changed\nMIME-Version: 1.0\nContent-Type: text/plain; charset=utf-8\nContent-Transfer-Encoding: 8bit\n\n' \
        "$user" ${recipient} "$PANOPTIKON_WATCHER"
      cat
    } | ${lib.getExe pkgs.msmtp} \
      --host=mail.cock.li --port=587 --tls=on --auth=on \
      --user="$user" --from="$user" \
      --passwordeval='${pkgs.coreutils}/bin/cut -d: -f2- ${credentials}' \
      ${recipient}
  '';
in
{
  age.secrets.pr-notifier-smtp.owner = "panoptikon";

  services.panoptikon = {
    enable = true;
    watchers = lib.mapAttrs (_: watcher: watcher // { reporters = [ mail ]; }) {
      "github-meta".script = pkgs.panoptikonWatchers.json {
        jqScript = ''
          {
            ssh_key_fingerprints: .ssh_key_fingerprints,
            ssh_keys: .ssh_keys
          }
        '';
      } "https://api.github.com/meta";
      kobudo-tesshinkan.script = pkgs.panoptikonWatchers.html "https://kobudo-tesshinkan.eu/index.php/de/termine-berichte/lehrgaenge/";
      carolinawelslau.script = pkgs.panoptikonWatchers.htmlSelector "#main" "https://carolinawelslau.de/";
      humboldt-preis.script = pkgs.panoptikonWatchers.htmlSelector "#content-core" "https://www.hu-berlin.de/de/ueberblick/menschen/ehrungen/humboldtpreis";
      lisalittmann.script = pkgs.panoptikonWatchers.htmlSelector "#site-content" "https://lisalittmann.de/";
      lisalittmann-archive.script = pkgs.panoptikonWatchers.htmlSelector "#site-content" "https://lisalittmann.de/archive/";
      lisalittmann-projects.script = pkgs.panoptikonWatchers.htmlSelector "#site-content" "https://lisalittmann.de/projects/";
      tatort.script = pkgs.panoptikonWatchers.htmlSelector ".linklist" "https://www.daserste.de/unterhaltung/krimi/tatort/sendung/index.html";
      warpgrid-idiomarium.script = pkgs.panoptikonWatchers.htmlSelector "#site-content" "https://warpgrid.de/idiomarium/";
      warpgrid-futurism.script = pkgs.panoptikonWatchers.htmlSelector "#site-content" "https://warpgrid.de/futurism/";
      warpgrid-imagiary.script = pkgs.panoptikonWatchers.htmlSelector "#site-content" "https://warpgrid.de/imagiary/";
      warpgrid-alchemy.script = pkgs.panoptikonWatchers.htmlSelector "#site-content" "https://warpgrid.de/alchemy/";
      indogermanische-forschungen.script = pkgs.panoptikonWatchers.htmlSelector "#latestIssue" "https://www.degruyter.com/journal/key/INDO/html";
      ig-neuigkeiten.script = pkgs.panoptikonWatchers.htmlSelector "[itemprop=articleBody]" "https://www.indogermanistik.org/aktuelles/neuigkeiten.html";
      ig-tagungen.script = pkgs.panoptikonWatchers.htmlSelector "[itemprop=articleBody]" "https://www.indogermanistik.org/tagungen/tagungen-der-ig.html";
      fu-distant.script = pkgs.panoptikonWatchers.htmlSelector "#current_events" "https://www.geschkult.fu-berlin.de/en/e/ma-distant/Termine/index.html";
      fu-aegyptologie.script = pkgs.panoptikonWatchers.htmlSelector "#current_events" "https://www.geschkult.fu-berlin.de/e/aegyptologie/termine/index.html";
    };
  };
}
