{ pkgs, config, ... }:
{
  imports = [ <modules/git.nix> ];

  # niveum.git = {
  #   enable = true;
  #   repositories.niveum = {
  #     enable = true;
  #     location = "/tmp/niveum";
  #     branches = [ "master" ];
  #     remotes.origin = "git@github.com:kmein/niveum";
  #   };
  # };

  environment.systemPackages = [
    pkgs.git
    pkgs.gitAndTools.hub
    pkgs.gitAndTools.git-extras
    pkgs.gitstats
    pkgs.patch
    pkgs.patchutils
    pkgs.git-quick-stats
  ];

  home-manager.users.me.programs.git = {
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
      st = "status";
      unstage = "reset HEAD --";
      diffs = "diff --staged";
      last = "log -1 HEAD";
      logs = "log --pretty=oneline";
      graph = "log --graph --abbrev-commit --decorate --date=relative --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all";
    };
    ignores = config.niveum.ignore;
  };
}
