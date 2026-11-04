{ pkgs, ... }:
let
  domain = "share.${pkgs.lib.niveum.domain}";
  root = "/var/www/${domain}";
in
{
  # filled by hand (rsync to root@makanek); not backed up, the files' source lives elsewhere
  systemd.tmpfiles.settings.share.${root}.d.mode = "0755";

  # one directory per share, each guarded by the .htpasswd inside it, so a new
  # share is an rsync, not a deploy
  services.nginx.virtualHosts.${domain} = {
    forceSSL = true;
    enableACME = true;
    inherit root;
    extraConfig = "charset UTF-8;";
    locations = {
      # a listing would name every share
      "= /".return = "404";
      # .htpasswd itself; before the share block, which would match it too
      "~ /\\." = {
        priority = 900;
        return = "404";
      };
      # without a .htpasswd nobody gets in
      "~ ^/([^/]+)".extraConfig = ''
        auth_basic "${domain}";
        auth_basic_user_file ${root}/$1/.htpasswd;
        autoindex on;
      '';
    };
  };
}
