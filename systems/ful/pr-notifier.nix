{ config, pkgs, ... }:
let
  port = 9505;
  # Glob supports `*` and `?` only (no character classes), so years 20-24
  # are spelled out explicitly. Hides every release branch before 25.xx.
  oldYears = ["1?" "20" "21" "22" "23" "24"];
  branchBlacklist = pkgs.writeText "pr-notifier-branch-blacklist" (
    builtins.concatStringsSep "\n" (
      builtins.concatMap (y: [
        "release-${y}.??"
        "nixos-${y}.??"
        "nixos-${y}.??-small"
        "nixpkgs-${y}.??-darwin"
        "staging-${y}.??"
      ]) oldYears
    )
  );
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
    branchBlacklistFile = branchBlacklist;
  };

  services.nginx.virtualHosts."pr.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:${toString port}";
    };
  };
}
