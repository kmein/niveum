{ pkgs, config, ... }:
{
  environment.systemPackages = [
    pkgs.mr
    pkgs.git
    pkgs.gitAndTools.hub
    pkgs.gitAndTools.git-extras
    pkgs.gitstats
    pkgs.patch
    pkgs.patchutils
    pkgs.git-quick-stats
  ];

  home-manager.users.me = {
    home.file.".mrconfig".text = ''
      [DEFAULT]
      git_gc = git gc "$@"

      [prog/git/blog]
      checkout = git clone 'git@github.com:kmein/blog' 'blog'

      [prog/git/brockman]
      checkout = git clone 'git@github.com:kmein/brockman.git' 'brockman'

      [prog/git/menstruation.rs]
      checkout = git clone 'git@github.com:kmein/menstruation.rs.git' 'menstruation.rs'

      [prog/git/menstruation-telegram]
      checkout = git clone 'git@github.com:kmein/menstruation-telegram' 'menstruation-telegram'

      [prog/git/meteora]
      checkout = git clone 'git@github.com:kmein/meteora.git' 'meteora'

      [prog/git/niveum]
      checkout = git clone 'git@github.com:kmein/niveum' 'niveum'

      [prog/git/poetry]
      checkout = git clone 'git@github.com:kmein/poetry' 'poetry'

      [prog/git/mnemosyne]
      checkout = git clone 'git@github.com:kmein/mnemosyne' 'mnemosyne'

      [prog/git/quotes]
      checkout = git clone 'git@github.com:kmein/quotes' 'quotes'

      [prog/git/stockholm]
      checkout = git clone 'https://cgit.krebsco.de/stockholm' 'stockholm'

      [prog/git/telebots]
      checkout = git clone 'git@github.com:kmein/telebots' 'telebots'

      [prog/git/traadfri]
      checkout = git clone 'git@github.com:kmein/traadfri' 'traadfri'

      [prog/git/zen]
      checkout = git clone 'git@github.com:kmein/zen.git' 'zen'

      [prog/git/sphinx]
      checkout = git clone 'git@github.com:kmein/sphinx.git' 'sphinx'
    '';

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
        graph = "log --graph --abbrev-commit --decorate --date=relative --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all";
      };
      ignores = config.niveum.ignore;
    };
  };
}
