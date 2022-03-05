{ pkgs, config, lib, ... }:
let
  inherit (import <niveum/lib>) kieran;

  urlwatchDir = "/var/lib/urlwatch";

  urlsFile = pkgs.writeText "urls" (builtins.concatStringsSep "\n---\n" (map builtins.toJSON urls));

  urls = [
    {
      name = "Corona-Verordnung";
      url = "https://www.berlin.de/corona/massnahmen/verordnung/";
      filter = [ { css = "[role=main]"; } "html2text" "strip" ];
    }
    {
      name = "HU Semester";
      url = "https://agnes.hu-berlin.de/lupo/rds?state=change&type=6&moduleParameter=semesterSelect&nextdir=change&next=SearchSelect.vm&subdir=applications&targettype=7&targetstate=change&getglobal=semester";
      filter = [ { css = "fieldset"; } "html2text" "strip" ];
    }
    {
      name = "Lammla 2021";
      url = "http://lammla.info/index.php?reihe=30";
      filter = [ "html2text" "strip" ];
    }
    {
      name = "Tatort";
      url = "https://www.daserste.de/unterhaltung/krimi/tatort/vorschau/index.html";
      filter = [
        "html2text"
        "strip"
        {
          shellpipe = ''
            ${pkgs.gnused}/bin/sed 's/&#32;/ /g;s/))/&\n/g;s/ \+/ /g'
          '';
        }
      ];
    }
    {
      name = "Kratylos";
      url = "https://kratylos.reichert-online.org/current_issue/KRATYLOS";
      filter = [ { element-by-id = "content"; } "html2text" "strip" ];
    }
    {
      name = "Zeno Free E-Books";
      url = "http://www.zeno.org/Lesesaal/M/E-Books";
      filter = [ { element-by-class = "zenoCOMain"; } "html2text" "strip" ];
    }
    {
      name = "Arnshaugk Neuerscheinungen";
      url = "http://www.arnshaugk.de/index.php";
      filter = [ "html2text" "strip" ];
    }
    {
      name = "Carolina Welslau";
      url = "https://carolinawelslau.de/";
      filter = [ { element-by-id = "main"; } "html2text" "strip" ];
    }
    {
      name = "Lisa Littmann";
      url = "https://lisalittmann.de/";
      filter = [ { element-by-id = "main"; } "html2text" "strip" ];
    }
    {
      name = "Lisa Littmann: Projects";
      url = "https://lisalittmann.de/projects/";
      filter = [ { element-by-id = "main"; } "html2text" "strip" ];
    }
    {
      name = "Lisa Littmann: Archive";
      url = "https://lisalittmann.de/archive/";
      filter = [ { element-by-id = "main"; } "html2text" "strip" ];
    }
    {
      name = "WarpGrid: Idiomarium";
      url = "https://warpgrid.de/idiomarium/";
      filter = [ { element-by-id = "site-content"; } "html2text" "strip" ];
    }
    {
      name = "WarpGrid: Futurism";
      url = "https://warpgrid.de/futurism/";
      filter = [ { element-by-id = "site-content"; } "html2text" "strip" ];
    }
    {
      name = "WarpGrid: Imagiary";
      url = "https://warpgrid.de/imagiary/";
      filter = [ { element-by-id = "site-content"; } "html2text" "strip" ];
    }
    {
      name = "WarpGrid: Cook";
      url = "https://warpgrid.de/alchemy/";
      filter = [ { element-by-id = "site-content"; } "html2text" "strip" ];
    }
    {
      name = "Indogermanische Forschungen";
      url = "https://www.degruyter.com/journal/key/INDO/html";
      filter = [ { element-by-id = "latestIssue"; } "html2text" "strip" ];
    }
    {
      name = "IG Neuigkeiten";
      url = "https://www.indogermanistik.org/aktuelles/neuigkeiten.html";
      filter = [ { css = "[itemprop=articleBody]"; } "html2text" "strip" ];
    }
    {
      name = "IG Tagungen";
      url = "https://www.indogermanistik.org/tagungen/tagungen-der-ig.html";
      filter = [ { css = "[itemprop=articleBody]"; } "html2text" "strip" ];
    }
    {
      name = "Christian-Metz-Blamage";
      url = "https://www.deutschlandfunk.de/meine-nacht-schlaeft-nicht-pflanze-mich-nicht-in-dein-herz.700.de.html?dram:article_id=486475";
      filter = [ { element-by-class = "dlf-articledetail"; } "html2text" "strip" ];
    }
    {
      name = "fxght.or.flxght";
      url = "https://api.tellonym.me/profiles/name/fxght.or.flxght?limit=20";
      headers.tellonym-client = "web:0.52.0";
      filter = [
        {
          shellpipe = ''
            ${pkgs.jq}/bin/jq '.answers | map({
              question: .tell,
              answer: .answer,
              date: .createdAt,
              media: .media | map(.url)
            })'
          '';
        }
      ];
    }
  ];

  configFile = (pkgs.formats.yaml {}).generate "urlwatch.yaml" {
    display = {
      error = true;
      new = true;
      unchanged = false;
    };
    report = {
      email = {
        enabled = true;
        from = "2210@cock.li";
        html = false;
        method = "smtp";
        smtp = {
          host = "mail.cock.li";
          port = 587;
          starttls = true;
          auth = true;
          insecure_password = lib.strings.fileContents <secrets/mail/cock>;
        };
        subject = "{count} changes: {jobs}";
        to = kieran.email;
      };
      telegram = {
        enabled = false;
        bot_token = lib.strings.fileContents <system-secrets/telegram/kmein.token>;
        chat_id = "-1001504043752";
      };
      html.diff = "unified";
      stdout = {
        color = true;
        enabled = true;
      };
      text.footer = false;
      # telegram = {
      #   enabled = false;
      #   bot_token = lib.strings.fileContents <system-secrets/telegram/kmein.token>;
      #   chat_id = [ "18980945" ];
      # };
    };
  };
  urlwatch = pkgs.urlwatch.overrideAttrs (attrs: {
    patches = [ <niveum/packages/urlwatch-insecure.patch> ];
  });
in
{
  users.extraUsers.urlwatch = {
    home = urlwatchDir;
    createHome = true;
    isSystemUser = true;
    group = "urlwatch";
  };

  users.groups.urlwatch = {};

  systemd.services.urlwatch = {
    enable = true;
    startAt = "12:00";
    script = ''
      ${urlwatch}/bin/urlwatch \
        --config=${lib.escapeShellArg configFile} \
        --urls=${lib.escapeShellArg urlsFile}
    '';
    serviceConfig = {
      User = config.users.extraUsers.urlwatch.name;
      Group = config.users.groups.urlwatch.name;
      WorkingDirectory = config.users.extraUsers.urlwatch.home;
      PermissionsStartOnly = "true";
      PrivateTmp = "true";
      SyslogIdentifier = "urlwatch";
      Type = "oneshot";
    };
  };
}
