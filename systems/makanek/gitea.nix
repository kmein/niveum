let
  inherit (import <niveum/lib>) sshPort;
  domain = "https://code.kmein.de";
in {
  services.gitea = {
    enable = true;
    disableRegistration = true;
    rootUrl = domain;
    appName = "code.kmein.de";
    ssh.clonePort = sshPort;
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
