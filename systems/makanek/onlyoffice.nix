{config, ...}: {
  services.onlyoffice = {
    enable = true;
    port = 8111;
    hostname = "onlyoffice.kmein.de";
    jwtSecretFile = config.age.secrets.onlyoffice-key.path;
  };

  age.secrets.onlyoffice-key = {
    file = ../../secrets/onlyoffice-jwt-key.age;
    owner = "onlyoffice";
  };

  # otherwise this leads to nginx
  # open() "/var/lib/onlyoffice/documentserver/App_Data/cache/files/data/conv_check_1138411943_docx/output.docx" failed (13: Permission denied)
  # and mysterious 403 errors
  system.activationScripts.onlyoffice-readable.text = ''
    chmod a+x /var/lib/onlyoffice/documentserver/
  '';

  services.nginx.virtualHosts.${config.services.onlyoffice.hostname} = {
    enableACME = true;
    forceSSL = true;
  };
}
