{ pkgs, ... }:
let
  get-todo-dir = pkgs.writers.writeDash "git-toplevel-todo" ''
    if GIT_TOPLEVEL=$(${pkgs.git}/bin/git rev-parse --show-toplevel 2>/dev/null); then
      echo "$GIT_TOPLEVEL/.todo"
    else
      echo "$HOME/cloud/Dropbox/todo"
    fi
  '';
  todo-txt-config = pkgs.writeText "todo.cfg" ''
    export TODO_DIR="$(${get-todo-dir})"

    export TODO_FILE="$TODO_DIR/todo.txt"
    export DONE_FILE="$TODO_DIR/done.txt"
    export REPORT_FILE="$TODO_DIR/report.txt"
  '';
in {
  environment = {
    systemPackages = [ pkgs.todo-txt-cli ];
    shellAliases.t = "todo.sh -d ${todo-txt-config}";
    variables.TODOTXT_DEFAULT_ACTION = "ls";
  };
}
