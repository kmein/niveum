{ config, ... }:
let
  port = 9505;
in
{
  services.pr-notifier = {
    enable = true;
    inherit port;
    domain = "https://pr.kmein.de";
    smtpCredentialsFile = config.age.secrets.pr-notifier-smtp.path;
    smtpHost = "mail.cock.li";
    githubTokenFile = config.age.secrets.pr-notifier-github.path;
    smtpFrom = "sf5vpci6clp1qhbeu37rlir57s6n7lu8@airmail.cc";
    emailFromName = "❄ PR Patrol";
  };

  services.nginx.virtualHosts."pr.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:${toString port}";
    };
  };
}
