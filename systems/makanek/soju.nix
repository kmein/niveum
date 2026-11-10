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
  # the DynamicUser's state; /var/lib/soju is only a symlink to it
  stateDir = "/var/lib/private/soju";
  brockmanLogs = "${stateDir}/logs/${nick}/brockman";
  realname = lib.head (lib.splitString " " pkgs.lib.niveum.kieran.name);
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
  arg = lib.escapeShellArg;
  run = "sojuctl user run ${nick}";
  setupNetwork = name: network: ''
    if ! ${run} network status | cut -d ' ' -f 1 | grep -qxF ${name}; then
      ${run} network create -name ${name} -addr ${arg network.addr} -enabled false
    fi
    # before connecting, so the first connection already authenticates
    password=$(sasl ${name})
    ${run} sasl set-plain -network ${name} ${nick} "$password"
    ${run} network update ${name} -addr ${arg network.addr} -nick ${nick} -realname ${arg realname} -enabled true
    channels=$(${run} channel status -network ${name} | cut -d ' ' -f 1)
    for channel in ${lib.escapeShellArgs network.channels}; do
      printf '%s\n' "$channels" | grep -qxF -- "$channel" || ${run} channel create "$channel/${name}"
    done
  '';
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

  # brings soju's database in line with the networks and channels above;
  # whatever a client adds stays. sojuctl takes passwords only as arguments,
  # so they show in /proc for the moment each call runs.
  systemd.services.soju-setup = {
    description = "Declare soju's networks and channels";
    requires = [ "soju.service" ];
    after = [ "soju.service" ];
    partOf = [ "soju.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      sojuctl() {
        ${lib.getExe' config.services.soju.package "sojuctl"} -config ${config.services.soju.configFile} "$@"
      }
      # lines of `<network> <password>`
      sasl() {
        password=$(sed -n "s/^$1 //p" ${config.age.secrets.soju-sasl.path})
        [ -n "$password" ] || { echo "no sasl password for $1" >&2; return 1; }
        printf '%s' "$password"
      }

      # the admin socket appears only once soju runs
      for _ in $(seq 100); do
        sojuctl server status >/dev/null 2>&1 && break
        sleep 0.1
      done

      password=$(cat ${config.age.secrets.soju-password.path})
      if sojuctl user status | cut -d ' ' -f 1 | grep -qxF ${nick}; then
        sojuctl user update ${nick} -password "$password"
      else
        sojuctl user create -username ${nick} -password "$password" -admin -nick ${nick} -realname ${arg realname}
      fi

      ${lib.concatStrings (lib.mapAttrsToList setupNetwork networks)}
    '';
  };

  age.secrets = {
    soju-password.file = ../../secrets/soju-password.age;
    soju-sasl.file = ../../secrets/soju-sasl.age;
  };

  # soju logs every channel or none, and the brockman network's channels are
  # feed firehoses: keep only a day of them, enough for catching up. by mtime,
  # as chathistory reads bump the atime.
  systemd.tmpfiles.settings.soju.${brockmanLogs}.e.age = "m:1d";

  services.restic.backups.niveum = {
    paths = [
      stateDir
      # weechat's logs, from before soju
      "/var/lib/weechat/logs"
    ];
    exclude = [ brockmanLogs ];
  };
}
