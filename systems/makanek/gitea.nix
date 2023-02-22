let
  inherit (import ../../lib) sshPort;
  domain = "https://code.kmein.de";
in {
  services.gitea = {
    enable = true;
    rootUrl = domain;
    appName = "code.kmein.de";
    settings = {
      server.SSH_PORT = sshPort;
      service.DISABLE_REGISTRATION = true;
    };
  };
  services.nginx.virtualHosts."code.kmein.de" = {
    forceSSL = true;
    enableACME = true;
    locations."/".extraConfig = "proxy_pass http://localhost:3000;";
  };

  niveum.passport.services = [
    {
      link = domain;
      title = "Gitea";
      description = "hosts a couple of <tt>git</tt> repos. Registration is disabled.";
    }
  ];
}
