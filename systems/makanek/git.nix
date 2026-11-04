# code.kmein.de: bare repositories pushed over ssh, published through cgit
{
  config,
  lib,
  pkgs,
  ...
}:
let
  domain = "code.${pkgs.lib.niveum.domain}";
  privateDomain = "private.${domain}";
  root = config.users.users.git.home;
  # nginx again, behind anubis
  upstream = {
    addr = "127.0.0.1";
    port = 3000;
  };
  # listed: browsable and clonable by anyone. every other repository in `root` needs the password.
  # grouped by cgit section, which lists "" without a heading first, then the others by name
  public = {
    "" = {
      ashram = "आश्रमः 'hermitage' | living the GNU ascetic's life 🛕";
      pr-notifier = "get an email once your PR lands";
      sgraebbel = "multiplayer Scrabble in Gleam, htmx and SQLite";
      syrinx = "σῦριγξ 'pipe' | audio tools";
      to-hen = "τὸ ἕν 'the one' | Plotinian monorepo / working with the garage door up";
      typst-tex = "LaTeX's article class, reproduced in Typst to within 0.05pt";
      vibe = "natural language compiler and interpreter";
      leitner = "orthodox implementation of Leitner's spaced repetition";
      recht = "German law parser";
      sis = "k9s for systemd";
    };
    "astrology tools" = {
      ephemeris-service = "astrology from the Swiss Ephemeris: service, CLI, tray";
      libswisseph-sys = "Swiss Ephemeris bindings for Rust, patched to build with GCC 14";
    };
    "epub tools" = {
      epub-digest = "epub digests of starred miniflux and wallabag articles, for an e-reader";
      man-epub = "epubs of man pages, vim help, info and nix manuals, for an e-reader";
      coptic-ebooks = "epubs of coptic texts";
    };
    nixos = {
      niphas = "νιφάς 'snowflake' | portable configuration";
      niveum = "NixOS configuration";
      nixos-nspawn-oci = "NixOS module running OCI images as systemd-nspawn machines, pulled with importctl";
      nsl = "NixOS Subsystem for Linux: other distributions as systemd-nspawn machines";
      panoptikon = "watch the world from NixOS";
    };
  };
  repos = lib.concatMapAttrs (
    section:
    lib.mapAttrs (
      name: desc:
      {
        path = "${root}/${name}.git";
        desc =
          desc
          +
            lib.optionalString (mirrors ? ${name})
              " (mirror of ${lib.removePrefix "https://" mirrors.${name}})";
      }
      // lib.optionalAttrs (section != "") { inherit section; }
    )
  ) public;
  # pushed to github and fetched from there, the repository created on the first run
  mirrors = {
    niveum = "https://github.com/kmein/niveum";
    nsl = "https://github.com/kmein/nsl";
    recht = "https://github.com/kmein/recht";
    coptic-ebooks = "https://github.com/kmein/coptic-ebooks";
    leitner = "https://github.com/kmein/leitner";
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

  # git-shell runs nothing else: `ssh code.kmein.de repo-new <name>` creates a private repository
  systemd.tmpfiles.rules = [
    (pkgs.lib.niveum.tmpfilesConfig {
      type = "L+";
      path = "${root}/git-shell-commands";
      argument = pkgs.linkFarm "git-shell-commands" {
        repo-new = pkgs.writers.writeDash "repo-new" ''
          set -efu
          if [ $# -ne 1 ]; then
            echo "usage: repo-new <name>" >&2
            exit 1
          fi
          name=$1
          case $name in
            [!a-z0-9]* | *[!a-z0-9._-]* | *.git)
              echo "repo-new: $name: use [a-z0-9._-], starting with a letter or digit, without .git" >&2
              exit 1
              ;;
          esac
          repo=${root}/$name.git
          if [ -e "$repo" ]; then
            echo "repo-new: $name exists" >&2
            exit 1
          fi
          ${lib.getExe pkgs.git} init --quiet --bare --initial-branch=main "$repo"
          echo "${domain}:$name.git"
        '';
        # without it, git-shell-commands would also enable an interactive login
        no-interactive-login = pkgs.writers.writeDash "no-interactive-login" ''
          echo "${domain} only speaks git, and repo-new <name>" >&2
          exit 128
        '';
      };
    })
  ];

  services.restic.backups.niveum.paths = [ root ];

  systemd.services.git-mirror = {
    startAt = "hourly";
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    path = [ pkgs.git ];
    serviceConfig = {
      User = "git";
      Group = "git";
    };
    # one failing upstream must not keep the others from updating
    script = ''
      mirror() {
        repo=${root}/$1.git
        # upstream's default branch, so cgit and clones start on it; asking first also
        # keeps a mistyped url from leaving an empty repository behind
        head=$(git ls-remote --symref "$2" HEAD | sed -n 's|^ref: \(refs/heads/.*\)\tHEAD$|\1|p') &&
          [ -n "$head" ] &&
          { [ -d "$repo" ] || git init --quiet --bare "$repo"; } &&
          git -C "$repo" fetch --quiet --prune "$2" '+refs/heads/*:refs/heads/*' '+refs/tags/*:refs/tags/*' &&
          git -C "$repo" symbolic-ref HEAD "$head"
      }
      failed=0
      ${lib.concatStrings (
        lib.mapAttrsToList (name: url: ''
          mirror ${name} ${url} || { echo "${name}: mirroring ${url} failed" >&2; failed=1; }
        '') mirrors
      )}
      exit $failed
    '';
  };

  # public: what `public` lists, browsable and clonable by anyone
  services.cgit.code = {
    enable = true;
    nginx.virtualHost = "code-upstream";
    # the owner, so git's ownership check passes; the sandbox below makes it read-only
    user = "git";
    group = "git";
    inherit repos;
    # git-http-backend serves only the links the module makes from `repos`
    gitHttpBackend.checkExportOkFiles = false;
    settings = {
      root-title = "ἐργαστήριον";
      root-desc = "working with the garage door up";
      # 80 by default, which cuts off the longer descriptions and the mirror note
      max-repodesc-length = 120;
      # where patches go, on every page instead of "generated by cgit"
      footer = "${pkgs.writeText "cgit-footer.html" ''
        <div class='footer'>patches by email to <a href='mailto:${pkgs.lib.niveum.kieran.email}'>${pkgs.lib.niveum.kieran.email}</a>,
        from <code>git format-patch</code> or <code>git send-email</code>;
        for mirrors of github, pull requests there</div>
      ''}";
      clone-url = "https://${domain}/$CGIT_REPO_URL";
      readme = [
        ":README.md"
        ":README"
      ];
      about-filter = "${pkgs.cgit}/lib/cgit/filters/about-formatting.sh";
      source-filter = "${pkgs.cgit}/lib/cgit/filters/syntax-highlighting.py";
    };
  };

  # private: every repository in `root`, behind the code password
  services.cgit.private = {
    enable = true;
    nginx.virtualHost = privateDomain;
    user = "git";
    group = "git";
    scanPath = root;
    gitHttpBackend.checkExportOkFiles = false;
    settings = {
      root-title = "ἐργαστήριον";
      root-desc = "everything, public or not";
      # <name>, not <name>.git, as on the public side
      remove-suffix = true;
      clone-url = "https://${privateDomain}/$CGIT_REPO_URL";
      readme = [
        ":README.md"
        ":README"
      ];
      about-filter = "${pkgs.cgit}/lib/cgit/filters/about-formatting.sh";
      source-filter = "${pkgs.cgit}/lib/cgit/filters/syntax-highlighting.py";
    };
  };

  systemd.services.fcgiwrap-cgit-code.serviceConfig.ReadOnlyPaths = root;
  systemd.services.fcgiwrap-cgit-private.serviceConfig.ReadOnlyPaths = root;

  age.secrets.code-basicAuth = {
    file = ../../secrets/code-basicAuth.age;
    owner = "nginx";
    group = "nginx";
    mode = "400";
  };

  # one shared password and bcrypt behind it: slow down guessing, per client address
  services.nginx.appendHttpConfig = ''
    limit_req_zone $binary_remote_addr zone=private-code:1m rate=2r/s;
  '';

  services.nginx.virtualHosts.${privateDomain} = {
    forceSSL = true;
    enableACME = true;
    basicAuthFile = config.age.secrets.code-basicAuth.path;
    # a page with its stylesheet and icon, or a clone, stays well inside the burst
    extraConfig = ''
      limit_req zone=private-code burst=20 nodelay;
      limit_req_status 429;
    '';
  };

  services.nginx.virtualHosts.code-upstream = {
    serverName = domain;
    listen = [ upstream ];
    extraConfig = ''
      # the client asked anubis on https, not this port
      absolute_redirect off;
      # before any location: the backend serves <name>, so git asking for gitea's
      # /<owner>/<name> or for <name>.git is pointed there
      rewrite ^/(?:kfm|fable|krebs)/([^/]+?)(?:\.git)?/(info/refs|git-upload-pack)$ /$1/$2 last;
      rewrite ^/([^/]+)\.git/(info/refs|git-upload-pack)$ /$1/$2 last;
    '';
    locations = {
      # gitea's web urls: browsers are sent to the matching cgit page
      "~ ^/(?:kfm|fable|krebs)(?:/|$)".extraConfig = ''
        rewrite ^/[^/]+/([^/]+)/src/branch/([^/]+)/(.*)$ /$1/tree/$3?h=$2 permanent;
        rewrite ^/[^/]+/([^/]+)/raw/branch/([^/]+)/(.*)$ /$1/plain/$3?h=$2 permanent;
        rewrite ^/[^/]+/([^/]+)/commit/([0-9a-f]+)$ /$1/commit/?id=$2 permanent;
        rewrite ^/[^/]+/([^/]+?)(?:\.git)?(?:/.*)?$ /$1/ permanent;
        rewrite ^ / permanent;
      '';
    };
  };

  services.anubis.instances.code = {
    # no metrics server at all: anubis ignores an empty METRICS_BIND and falls back
    # to its built-in default, only the flag turns it off
    extraFlags = [ "-metrics-bind \"\"" ];
    settings = {
      BIND = "/run/anubis/anubis-code/anubis.sock";
      METRICS_BIND = "";
      TARGET = "http://${upstream.addr}:${toString upstream.port}";
      OG_PASSTHROUGH = true;
      SERVE_ROBOTS_TXT = true;
    };
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
    };
  };
}
