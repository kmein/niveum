{ pkgs, config, lib, ... }: {
  environment.systemPackages = [
    pkgs.mr
    pkgs.git
    pkgs.gitAndTools.gitflow
    pkgs.gitAndTools.hub
    pkgs.gitAndTools.gh
    pkgs.gitAndTools.git-extras
    pkgs.unstable.gitAndTools.git-trim
    pkgs.gitstats
    pkgs.patch
    pkgs.patchutils
    pkgs.git-preview
  ];

  environment.shellAliases = {
    gf = "git-flow";
    g = "git";
  };

  home-manager.users.me = {
    home.file.".mrconfig".text = let
      prependPath = prefix:
        lib.attrsets.mapAttrs'
        (path: lib.attrsets.nameValuePair "${prefix}/${path}");
      git = url: { checkout = "git clone ${url}"; };
      github = owner: repo: git "git@github.com:${owner}/${repo}";
      keybase = owner: repo: git "keybase://private/${owner}/${repo}";
    in lib.generators.toINI { } ({
      DEFAULT = { git_gc = ''git gc "$@"''; };
    } // prependPath "projects" {
      "menstruation.rs" = github "kmein" "menstruation.rs";
      brockman = github "kmein" "brockman";
      challenges = github "kmein" "challenges";
      conlangs = github "kmein" "conlangs";
      ledger = keybase "kmein" "ledger";
      mahlzeit = github "kmein" "mahlzeit";
      menstruation-telegram = github "kmein" "menstruation-telegram";
      meteora = github "kmein" "meteora";
      modernizr = github "kmein" "modernizr";
      niveum = github "kmein" "niveum";
      nixpkgs = github "NixOS" "nixpkgs";
      poetry = github "kmein" "poetry";
      quotes = github "kmein" "quotes";
      sphinx = github "kmein" "sphinx";
      stockholm = git "https://cgit.krebsco.de/stockholm";
      telebots = github "kmein" "telebots";
      traadfri = github "kmein" "traadfri";
      wissen = github "kmein" "wissen";
      zen = github "kmein" "zen";
    });

    programs.git = {
      enable = true;
      package = pkgs.gitAndTools.gitFull;
      userName = config.niveum.user.name;
      userEmail = config.niveum.user.email;
      aliases = {
        br = "branch";
        co = "checkout";
        ci = "commit";
        cm = "commit -m";
        amend = "commit --amend";
        st = "status -s";
        unstage = "reset HEAD --";
        diffs = "diff --staged";
        last = "log -1 HEAD";
        logs = "log --pretty=oneline";
        graph =
          "log --graph --abbrev-commit --decorate --date=relative --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all";
      };
      ignores = config.niveum.ignore;
      extraConfig = {
        core.pager =
          "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -RFX";
        color = {
          ui = true;
          diff = {
            meta = "11";
            frag = "magenta bold";
            commit = "yellow bold";
            old = "red bold";
            new = "green bold";
            whitespace = "red reverse";
          };
          diff-highlight = {
            oldNormal = "red bold";
            oldHighlight = "red bold 52";
            newNormal = "green bold";
            newHighlight = "green bold 22";
          };
        };
      };
    };
  };
}
