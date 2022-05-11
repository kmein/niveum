let
  inherit (import <niveum/lib>) sshPort;
in {
  services.gitea = {
    enable = true;
    disableRegistration = true;
    rootUrl = "https://code.kmein.de";
    appName = "code.kmein.de";
    ssh.clonePort = sshPort;
  };
  services.nginx.virtualHosts."code.kmein.de" = {
    forceSSL = true;
    enableACME = true;
    locations."/".extraConfig = "proxy_pass http://localhost:3000;";
  };
}
