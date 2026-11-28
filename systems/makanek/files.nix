{ pkgs, ... }:
let
  domain = "files.${pkgs.lib.niveum.domain}";
  # the directory predates the name
  root = "/var/www/makanek.kmein.r";
in
{
  # filled by hand (rsync to root@makanek); not backed up, the files' source lives elsewhere
  systemd.tmpfiles.settings.files.${root}.d.mode = "0755";

  services.nginx.virtualHosts.${domain} = {
    inherit root;
    enableACME = true;
    forceSSL = true;
    extraConfig = pkgs.lib.niveum.wireguardOnly;
    locations."/".extraConfig = ''
      autoindex on;
      charset UTF-8;
    '';
  };
}
