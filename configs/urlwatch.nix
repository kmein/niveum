{ pkgs, config, lib, ... }:
let
  inherit (import <niveum/lib>) kieran;

  urlwatchDir = "/var/lib/urlwatch";

  urlsFile = pkgs.writeText "urls" (builtins.concatStringsSep "\n---\n" (map builtins.toJSON urls));

  urls = [
    {
      name = "Corona-Verordnung";
      url = "https://www.berlin.de/corona/massnahmen/verordnung/";
      filter = [
        {
          css = "[role=main]";
        }
        "html2text"
        "strip"
      ];
    }
    {
      name = "Christian-Metz-Blamage";
      url = "https://www.deutschlandfunk.de/meine-nacht-schlaeft-nicht-pflanze-mich-nicht-in-dein-herz.700.de.html?dram:article_id=486475";
      filter = [
        {
          element-by-id = "content";
        }
      ];
    }
  ];

  configFile = pkgs.writeText "urlwatch.yaml" (builtins.toJSON {
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
      html.diff = "unified";
      stdout = {
        color = true;
        enabled = true;
      };
      # telegram = {
      #   enabled = false;
      #   bot_token = lib.strings.fileContents <system-secrets/telegram/kmein.token>;
      #   chat_id = [ "18980945" ];
      # };
      # text = {
      #   details = true;
      #   footer = true;
      #   line_length = 75;
      # };
    };
  });
in
{
  users.extraUsers.urlwatch = {
    home = urlwatchDir;
    createHome = true;
  };

  systemd.services.urlwatch = {
    enable = true;
    startAt = "*-*-* 05:00:00";
    script = ''
      ${pkgs.urlwatch}/bin/urlwatch \
        --config=${lib.escapeShellArg configFile} \
        --urls=${lib.escapeShellArg urlsFile}
    '';
    serviceConfig = {
      User = config.users.extraUsers.urlwatch.name;
      WorkingDirectory = config.users.extraUsers.urlwatch.home;
      PermissionsStartOnly = "true";
      PrivateTmp = "true";
      SyslogIdentifier = "urlwatch";
      Type = "oneshot";
    };
  };
}
