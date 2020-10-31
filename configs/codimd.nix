{
  services.nginx.virtualHosts."pad.xn--kiern-0qa.de" = {
    enableACME = true;
    addSSL = true;
    locations."/".extraConfig = ''
      client_max_body_size 4G;
      proxy_set_header Host $host;
      proxy_pass http://localhost:3091;
    '';
  };

  services.codimd = {
    enable = true;
    configuration = {
      allowAnonymous = false;
      allowGravatar = false;
      db = {
        dialect = "sqlite";
        storage = "/var/lib/codimd/db.codimd.sqlite";
        useCDN = false;
      };
      port = 3091;
    };
  };
}
