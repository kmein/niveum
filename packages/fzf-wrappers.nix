{ write, lib, fzf, writers, findutils, procps, gnused, gawk, ripgrep }:
let
  wrappers.fe = writers.writeBash "fe" ''
    export PATH=$PATH:${fzf}/bin

    IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0))
    [[ -n "$files" ]] && ''${EDITOR:-vim} "''${files[@]}"
  '';
  wrappers.fkill = writers.writeDash "fkill" ''
    export PATH=$PATH:${procps}/bin:${gawk}/bin:${gnused}/bin:${fzf}/bin:${findutils}/bin

    if [ "$UID" != "0" ]; then
        pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
    else
        pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
    fi

    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill -''${1:-9}
    fi
  '';
  wrappers.vg = writers.writeBash "vg" ''
    export PATH=$PATH:${ripgrep}/bin:${fzf}/bin:${gawk}/bin

    file="$(rg "$@" | fzf -0 -1 | awk -F: '{print $1}')"

    if [[ -n $file ]]
    then
       ''${EDITOR:-vim} "$file"
    fi
  '';
in write "fzf-wrappers" (lib.mapAttrs' (name: link: lib.nameValuePair "/bin/${name}" {inherit link;}) wrappers)
