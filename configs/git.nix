{
  pkgs,
  inputs,
  ...
}: let
  inherit (import ../lib) kieran ignorePaths;
in {
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
    inputs.self.packages.${pkgs.system}.git-preview
  ];

  environment.shellAliases = {
    gf = "git-flow";
    g = "git";
  };

  home-manager.users.me = {
    programs.git = {
      enable = true;
      package = pkgs.gitFull;
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
        push.autoSetupRemote = true;

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
