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

  services.hedgedoc = {
    enable = true;
    configuration = {
      allowAnonymous = true;
      allowGravatar = false;
      allowFreeURL = true;
      db = {
        dialect = "sqlite";
        storage = "/var/lib/codimd/state.sqlite";
      };
      port = 3091;
    };
  };
}
