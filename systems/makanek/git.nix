# code.kmein.de: bare repositories pushed over ssh, published through cgit
{
  config,
  lib,
  pkgs,
  ...
}:
let
  domain = "code.${pkgs.lib.niveum.domain}";
  root = config.users.users.git.home;
  # the readme on the summary page too, not only on the about tab
  cgit = pkgs.cgit.overrideAttrs (old: {
    patches = old.patches or [ ] ++ [ ./cgit-summary-readme.patch ];
  });
  # nginx again, behind anubis
  upstream = {
    addr = "127.0.0.1";
    port = 3000;
  };
  # listed: browsable and clonable by anyone. every other repository in `root` needs the password
  public = {
    ashram = "आश्रमः 'hermitage' | living the GNU ascetic's life 🛕";
    ephemeris-service = "Astrology from the Swiss Ephemeris: service, CLI, tray";
    epub-digest = "epub digests of starred miniflux and wallabag articles, for an e-reader";
    libswisseph-sys = "Swiss Ephemeris bindings for Rust, patched to build with GCC 14";
    man-epub = "epubs of man pages, vim help, info and nix manuals, for an e-reader";
    niphas = "νιφάς ‘snowflake’ — portable configuration";
    niveum = "NixOS configuration";
    nixos-nspawn-oci = "NixOS module running OCI images as systemd-nspawn machines, pulled with importctl";
    panoptikon = "Watch the world from NixOS";
    pr-notifier = "Get an email once your PR lands";
    sgraebbel = "Multiplayer Scrabble in Gleam, htmx and SQLite";
    syrinx = "σῦριγξ – audio tools";
    to-hen = "τὸ ἕν: Plotinian monorepo / working with the garage door up / aren't we all just looking for the one?";
    vibe = "Natural language compiler and interpreter";
  };
in
{
  users.users.git = {
    isSystemUser = true;
    group = "git";
    home = "/var/lib/git";
    createHome = true;
    shell = "${pkgs.git}/bin/git-shell";
    openssh.authorizedKeys.keys = pkgs.lib.niveum.kieran.sshKeys;
  };
  users.groups.git = { };

  services.openssh.extraConfig = ''
    Match User git
      # git would print the banner on every push and fetch
      Banner none
  '';

  services.restic.backups.niveum.paths = [ root ];

  # github is where niveum is pushed to
  systemd.services.git-mirror-niveum = {
    startAt = "hourly";
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    serviceConfig = {
      User = "git";
      Group = "git";
    };
    script = ''
      ${lib.getExe pkgs.git} -C ${root}/niveum.git fetch --prune https://github.com/kmein/niveum \
        '+refs/heads/*:refs/heads/*' '+refs/tags/*:refs/tags/*'
    '';
  };

  services.cgit.code = {
    enable = true;
    nginx.virtualHost = "code-upstream";
    package = cgit;
    # the owner, so git's ownership check passes; the sandbox below makes it read-only
    user = "git";
    group = "git";
    repos = lib.mapAttrs (name: desc: {
      path = "${root}/${name}.git";
      inherit desc;
    }) public;
    # replaced below by one that also serves the private repositories, with a password
    gitHttpBackend.enable = false;
    settings = {
      root-title = "ἐργαστήριον";
      root-desc = "working with the garage door up";
      logo = "";
      clone-url = "https://${domain}/$CGIT_REPO_URL";
      readme = [
        ":README.md"
        ":README"
      ];
      about-filter = "${cgit}/lib/cgit/filters/about-formatting.sh";
      source-filter = "${cgit}/lib/cgit/filters/syntax-highlighting.py";
    };
  };

  systemd.services.fcgiwrap-cgit-code.serviceConfig.ReadOnlyPaths = root;

  age.secrets.code-basicAuth = {
    file = ../../secrets/code-basicAuth.age;
    owner = "nginx";
    group = "nginx";
    mode = "400";
  };

  services.nginx.appendHttpConfig = ''
    map $git_repo $git_auth {
      default "${domain}";
    ${lib.concatMapStrings (name: "  ${name} off;\n") (lib.attrNames public)}}
  '';

  services.nginx.virtualHosts.code-upstream = {
    serverName = domain;
    listen = [ upstream ];
    extraConfig = ''
      # the client asked anubis on https, not this port
      absolute_redirect off;
    '';
    locations = {
      "~ ^/(?<git_repo>[^/]+?)(?:\\.git)?/(?:info/refs|git-upload-pack)$" = {
        fastcgiParams = {
          SCRIPT_FILENAME = "${pkgs.git}/libexec/git-core/git-http-backend";
          GIT_PROJECT_ROOT = root;
          GIT_HTTP_EXPORT_ALL = "1";
          HOME = root;
          PATH_INFO = "$uri";
        };
        extraConfig = ''
          auth_basic $git_auth;
          auth_basic_user_file ${config.age.secrets.code-basicAuth.path};
          fastcgi_pass unix:${config.services.fcgiwrap.instances.cgit-code.socket.address};
        '';
      };
      # gitea's urls: git is served in place, browsers are sent to the matching cgit page
      "~ ^/(?:kfm|fable|krebs)(?:/|$)".extraConfig = ''
        rewrite ^/[^/]+/([^/]+?)(?:\.git)?/(info/refs|git-upload-pack)$ /$1/$2 last;
        rewrite ^/[^/]+/([^/]+)/src/branch/([^/]+)/(.*)$ /$1/tree/$3?h=$2 permanent;
        rewrite ^/[^/]+/([^/]+)/raw/branch/([^/]+)/(.*)$ /$1/plain/$3?h=$2 permanent;
        rewrite ^/[^/]+/([^/]+)/commit/([0-9a-f]+)$ /$1/commit/?id=$2 permanent;
        rewrite ^/[^/]+/([^/]+?)(?:\.git)?(?:/.*)?$ /$1/ permanent;
        rewrite ^ / permanent;
      '';
    };
  };

  services.anubis.instances.code.settings = {
    BIND = "/run/anubis/anubis-code/anubis.sock";
    METRICS_BIND = "/run/anubis/anubis-code/anubis-metrics.sock";
    TARGET = "http://${upstream.addr}:${toString upstream.port}";
    OG_PASSTHROUGH = true;
    SERVE_ROBOTS_TXT = true;
  };

  users.users.nginx.extraGroups = [ config.services.anubis.instances.code.group ];

  services.nginx.virtualHosts.${domain} = {
    forceSSL = true;
    enableACME = true;
    locations = {
      "/".proxyPass = "http://unix:${config.services.anubis.instances.code.settings.BIND}";
      # Hide Anubis' anime mascot. The challenge page loads it as a plain
      # <img> from this static path (embedded in the Anubis binary, so no file
      # to swap and de-branding it upstream means the paid BotStopper build).
      # Since the request is proxied through nginx, just answer it here with a
      # 1x1 transparent GIF instead of forwarding it to Anubis.
      "/.within.website/x/cmd/anubis/static/img/".extraConfig = "empty_gif;";
      "/metrics".proxyPass = "http://unix:${config.services.anubis.instances.code.settings.METRICS_BIND}";
    };
  };
}
