{ pkgs, ... }:
let todo-txt-config = pkgs.writeText "todo.cfg" ''
  export TODO_DIR="$HOME/cloud/Dropbox/todo"

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
