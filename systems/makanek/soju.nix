{
  config,
  lib,
  pkgs,
  ...
}:
let
  # an alias kartei lists for makanek, so ca.r signs it; nginx already holds
  # its certificate for files.nix
  domain = "makanek.kmein.r";
  port = 6697;
  nick = "kmein";
  setup = {
    user = nick;
    inherit nick;
    realname = lib.head (lib.splitString " " pkgs.lib.niveum.kieran.name);
    adminSocket = "/run/soju/admin";
    passwordFile = config.age.secrets.soju-password.path;
    # lines of `<network> <password>`
    saslFile = config.age.secrets.soju-sasl.path;
    networks = {
      hackint = {
        addr = "ircs://irc.hackint.org";
        channels = [
          "#eloop"
          "#krebs"
          "#the_playlist"
        ];
      };
      libera = {
        addr = "ircs://irc.libera.chat";
        channels = [
          "#haskell"
          "#binaergewitter"
          "#vim"
        ];
      };
      retiolum = {
        addr = "irc+insecure://irc.r";
        channels = [
          "#xxx"
          "#brockman"
          "#flix"
        ];
      };
      brockman = {
        addr = "irc+insecure://brockman.news";
        channels = [
          "#cook"
          "#kmeinung"
        ];
      };
    };
  };
in
{
  services.soju = {
    enable = true;
    hostName = domain;
    listen = [ ":${toString port}" ];
    tlsCertificate = "${config.security.acme.certs.${domain}.directory}/fullchain.pem";
    tlsCertificateKey = "${config.security.acme.certs.${domain}.directory}/key.pem";
    enableMessageLogging = true;
  };

  systemd.services.soju = {
    wants = [ "acme-${domain}.service" ];
    after = [ "acme-${domain}.service" ];
    # soju runs as a DynamicUser, which a static group can still be given
    serviceConfig.SupplementaryGroups = [ "irccert" ];
  };
  users.groups.irccert.members = [ "nginx" ];
  security.acme.certs.${domain} = {
    group = "irccert";
    # soju rereads its certificate on SIGHUP
    reloadServices = [ "soju" ];
  };

  # retiolum only; tls stays, as retiolum is other people's network too
  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ port ];

  systemd.services.soju-setup = {
    description = "Declare soju's networks and channels";
    requires = [ "soju.service" ];
    after = [ "soju.service" ];
    partOf = [ "soju.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${
        pkgs.writers.writePython3 "soju-setup" { flakeIgnore = [ "E501" ]; } ./soju-setup.py
      } ${pkgs.writeText "soju-setup.json" (builtins.toJSON setup)}";
    };
  };

  age.secrets = {
    soju-password.file = ../../secrets/soju-password.age;
    soju-sasl.file = ../../secrets/soju-sasl.age;
  };

  services.restic.backups.niveum.paths = [
    # the DynamicUser's state; /var/lib/soju is only a symlink to it
    "/var/lib/private/soju"
    # weechat's logs, from before soju
    "/var/lib/weechat/logs"
  ];
}
