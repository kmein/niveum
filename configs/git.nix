{
  pkgs,
  lib,
  ...
}:
{
  environment.systemPackages = [
    pkgs.mr
    pkgs.gitFull
    pkgs.git-crypt
    pkgs.gitflow
    pkgs.gh
    pkgs.git-extras
    # pkgs.git-trim
    pkgs.git-absorb
    pkgs.gitstats
    pkgs.patch
    pkgs.patchutils
  ];

  environment.shellAliases = {
    gf = "git-flow";
    g = "git";
  };

  home-manager.users.me = {
    programs.git = {
      enable = true;
      package = pkgs.gitFull;
      settings.alias = {
        br = "branch";
        co = "checkout";
        ci = "commit";
        cm = "commit -m";
        amend = "commit --amend";
        st = "status -s";
        unstage = "reset HEAD --";
        diffs = "diff --staged";
        pushf = "push --force-with-lease";
        last = "log -1 HEAD";
        logs = "log --pretty=oneline";
        graph = "log --graph --abbrev-commit --decorate --date=relative --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all";
      };
      ignores = pkgs.lib.niveum.ignorePaths;
      settings.user.name = pkgs.lib.niveum.kieran.name;
      settings.user.email = pkgs.lib.niveum.kieran.email;
      settings.pull.ff = "only";
      settings.rebase.autoStash = true;
      settings.merge.autoStash = true;
      settings.push.autoSetupRemove = true;
    };
  };
}
