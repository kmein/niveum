{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (pkgs.lib.niveum) domain;

  shareDomain = "share.${domain}";
  davDomain = "dav.${domain}";

  # A subdirectory of the storage box, mounted separately from the one
  # nextcloud uses: cifs presents a single fixed uid/gid for the whole mount,
  # and this one has to belong to syncthing rather than to nextcloud.
  mountPoint = "/mnt/share";

  # The syncthing folder root is the mount point; the nginx root is one level
  # below it, so that the htpasswd files in auth/ can be replicated by the same
  # folder while being unreachable over HTTP by construction.
  webRoot = "${mountPoint}/files";
  authRoot = "${mountPoint}/auth";

  zoteroRoot = "/var/lib/webdav";
  davPort = 8448;
in
{
  fileSystems.${mountPoint} = {
    device = "//u359050.your-storagebox.de/backup/share";
    fsType = "cifs";
    options = [
      "iocharset=utf8"
      "rw"
      "credentials=${config.age.secrets.hetzner-storagebox-credentials.path}"
      "uid=syncthing"
      "gid=nginx"
      "file_mode=0640"
      "dir_mode=0750"
      "seal"
      "mfsymlinks"
      "nofail"
      "x-systemd.automount"
    ];
  };

  # nginx serves the tree read-only; syncthing is the only writer.
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    cert = config.age.secrets.makanek-syncthing-cert.path;
    key = config.age.secrets.makanek-syncthing-key.path;
    settings = {
      devices = lib.filterAttrs (name: _: name != "makanek") pkgs.lib.niveum.syncthingIds;
      folders.${mountPoint} = {
        id = "share";
        label = "share";
        devices = [
          "manakish"
          "fatteh"
        ];
        # the laptops are the source of truth: a compromised web host must not
        # be able to push deletions back into them
        type = "receiveonly";
        # cifs presents fixed permissions and refuses chmod, so syncthing would
        # otherwise flag every file as out of sync
        ignorePerms = true;
        # no inotify over cifs; this is a share tree, not a hot directory
        rescanIntervalS = 300;
      };
    };
  };

  systemd.services.syncthing = {
    after = [ "mnt-share.mount" ];
    wants = [ "mnt-share.mount" ];
  };

  age.secrets = {
    makanek-syncthing-cert.file = ../../secrets/makanek-syncthing-cert.age;
    makanek-syncthing-key.file = ../../secrets/makanek-syncthing-key.age;
    webdav-zotero.file = ../../secrets/webdav-zotero.age;
  };

  # The point of this vhost: the access policy lives in the filesystem, not in
  # this file. Adding a password-protected share is a mkdir plus an htpasswd
  # file, both of which syncthing carries over — no rebuild, no deploy.
  services.nginx.virtualHosts.${shareDomain} = {
    enableACME = true;
    forceSSL = true;
    root = webRoot;
    extraConfig = ''
      charset utf-8;
      autoindex on;
      autoindex_exact_size off;
      autoindex_localtime on;
    '';
    locations = {
      # nothing interesting at the root, and listing it would enumerate every
      # capability-URL share
      "= /".return = "302 https://www.${domain}";

      # likewise: don't hand out the names of the protected shares
      "= /private/".extraConfig = "deny all;";

      # auth_basic_user_file takes variables, so this single block covers every
      # present and future private/<name>. No trailing slash in the pattern:
      # /private/foo must require auth too, not just /private/foo/.
      "~ ^/private/([^/]+)".extraConfig = ''
        auth_basic "${shareDomain}";
        auth_basic_user_file ${authRoot}/$1;
      '';

      # .stfolder, .stversions and friends
      "~ /\\.".extraConfig = "deny all;";
    };
  };

  # Zotero attachment sync speaks WebDAV (PROPFIND/MKCOL/PUT), which the static
  # vhost above cannot answer. Kept as its own daemon rather than rebuilding
  # nginx with nginxModules.dav: makanek's nginx fronts gitea, hedgedoc, tt-rss
  # and grafana, and a custom build would drop all of them off the binary cache.
  services.webdav = {
    enable = true;
    environmentFile = config.age.secrets.webdav-zotero.path;
    settings = {
      address = "127.0.0.1";
      port = davPort;
      directory = zoteroRoot;
      permissions = "CRUD";
      users = [
        {
          username = "{env}WEBDAV_USERNAME";
          password = "{env}WEBDAV_PASSWORD";
        }
      ];
    };
  };

  systemd.tmpfiles.rules = [
    "d ${zoteroRoot} 0700 webdav webdav -"
  ];

  services.nginx.virtualHosts.${davDomain} = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString davPort}";
      extraConfig = ''
        # attachments are arbitrarily large, and buffering a 2 GB PDF to disk
        # before forwarding it helps nobody
        client_max_body_size 0;
        proxy_request_buffering off;
      '';
    };
  };

  # the share tree itself lives on the storage box and is replicated from the
  # laptops; zotero's is the only copy on this machine
  services.restic.backups.niveum.paths = [ zoteroRoot ];

  niveum.passport.services = [
    {
      title = "share";
      link = "https://${shareDomain}/public/";
      description = "shares files over plain HTTP, password-protected per directory.";
    }
  ];
}
