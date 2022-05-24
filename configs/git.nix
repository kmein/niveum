{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (import <niveum/lib>) kieran ignorePaths;
in {
  environment.systemPackages = [
    pkgs.mr
    pkgs.git
    pkgs.gitAndTools.gitflow
    pkgs.gitAndTools.gh
    pkgs.gitAndTools.git-extras
    pkgs.gitAndTools.git-trim
    pkgs.gitAndTools.git-absorb
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
    programs.git = {
      enable = true;
      package = pkgs.gitAndTools.gitFull;
      userName = kieran.name;
      userEmail = kieran.email;
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
      ignores = ignorePaths;
      extraConfig = {
        pull.ff = "only";
        rebase.autoStash = true;
        merge.autoStash = true;

        # # ref https://github.com/dandavison/delta
        # core.pager = "${pkgs.delta}/bin/delta";
        # interactive.diffFilter = "${pkgs.delta}/bin/delta --color-only";
        # delta.navigate = true;
        # merge.conflictStyle = "diff3";
        # diff.colorMoved = "default";
      };
    };
  };
}
