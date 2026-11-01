{
  config,
  pkgs,
  ...
}:
let
  domain = "ledger.${pkgs.lib.niveum.domain}";
  stateDir = config.services.hledger-web.stateDir;
in
{
  services.hledger-web = {
    enable = true;
    allow = "edit";
    serveApi = false; # serve only the JSON API
    baseUrl = "https://${domain}";
    journalFiles = [
      "privat.journal"
    ];
  };

  # the journals are private finances; keep them away from other service users
  systemd.services.hledger-web.serviceConfig = {
    StateDirectoryMode = "0700";
    UMask = "0077";
  };

  # stateDir is the hub of the ledger repo: laptops push into the checkout,
  # which updateInstead refuses while a web edit is uncommitted. git-shell
  # limits their keys to push and fetch.
  users.users.hledger = {
    shell = "${pkgs.git}/bin/git-shell";
    openssh.authorizedKeys.keys = pkgs.lib.niveum.kieran.sshKeys;
  };

  systemd.services.hledger-web.preStart = ''
    ${pkgs.git}/bin/git init --quiet --initial-branch=master
    ${pkgs.git}/bin/git config receive.denyCurrentBranch updateInstead
  '';

  systemd.paths.hledger-web-commit = {
    wantedBy = [ "paths.target" ];
    pathConfig.PathChanged = map (f: "${stateDir}/${f}") config.services.hledger-web.journalFiles;
  };

  systemd.services.hledger-web-commit = {
    path = [ pkgs.git ];
    # not in the repo config, where a copied .git would carry it to the laptops
    environment = {
      GIT_AUTHOR_NAME = "hledger-web";
      GIT_AUTHOR_EMAIL = "hledger-web@${config.networking.hostName}";
      GIT_COMMITTER_NAME = "hledger-web";
      GIT_COMMITTER_EMAIL = "hledger-web@${config.networking.hostName}";
    };
    script = ''
      git diff --quiet HEAD || git commit --all --quiet --message="web edit"
    '';
    serviceConfig = {
      Type = "oneshot";
      User = "hledger";
      Group = "hledger";
      WorkingDirectory = stateDir;
      UMask = "0077";
    };
  };

  services.restic.backups.niveum.paths = [ stateDir ];

  age.secrets = {
    ledger-basicAuth = {
      file = ../../secrets/ledger-basicAuth.age;
      owner = "nginx";
      group = "nginx";
      mode = "400";
    };
  };

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    basicAuthFile = config.age.secrets.ledger-basicAuth.path;
    forceSSL = true;
    locations."/".proxyPass = "http://127.0.0.1:${toString config.services.hledger-web.port}";
  };
}
