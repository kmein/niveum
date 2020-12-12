{ pkgs, ... }:
let
  get-todo-dir = pkgs.writers.writeDash "git-toplevel-todo" ''
    if GIT_TOPLEVEL=$(${pkgs.git}/bin/git rev-parse --show-toplevel 2>/dev/null); then
      echo "$GIT_TOPLEVEL/.todo"
    else
      echo "$HOME/cloud/Dropbox/todo"
    fi
  '';
in {
  home-manager.users.me.home.file.".todo/config".text = ''
    # export TODO_DIR="$(${get-todo-dir})"
    export TODO_DIR="$HOME/cloud/Dropbox/todo"

    export TODO_FILE="$TODO_DIR/todo.txt"
    export DONE_FILE="$TODO_DIR/done.txt"
    export REPORT_FILE="$TODO_DIR/report.txt"
  '';

  environment = {
    systemPackages = [ pkgs.todo-txt-cli ];
    shellAliases.t = "todo.sh";
    variables.TODOTXT_DEFAULT_ACTION = "ls";
  };
}
