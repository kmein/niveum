{ pkgs, ... }:
let
  # an alias kartei already lists for makanek, so every retiolum node resolves it
  domain = "makanek.kmein.r";
  root = "/var/www/${domain}";
in
{
  # filled by hand (rsync to root@makanek); not backed up, the files' source lives elsewhere
  systemd.tmpfiles.settings.files.${root}.d.mode = "0755";

  services.nginx.virtualHosts.${domain} = {
    inherit root;
    # port 80 is open to the internet, so a spoofed Host header must not get in
    extraConfig = with pkgs.lib.niveum; nginxAllow (retiolumNetwork ++ ownAddresses);
    locations."/".extraConfig = ''
      autoindex on;
      charset UTF-8;
    '';
  };
}
