{ symlinkJoin
, hledger
, writers
, lib
, git
, coreutils
, gnugrep
, timeLedger
, ...
}:
let
  date = "${coreutils}/bin/date +'%Y-%m-%d %H:%M:%S'";

  hora = writers.writeDashBin "hora" "${hledger}/bin/hledger -f ${lib.escapeShellArg timeLedger} \"$@\"";
  hora-edit = writers.writeDashBin "hora-edit" "$EDITOR ${lib.escapeShellArg timeLedger}";
  hora-status = writers.writeDashBin "hora-status" "${coreutils}/bin/tac ${lib.escapeShellArg timeLedger} | ${gnugrep}/bin/grep -m 1 .";

  hora-start = writers.writeDashBin "hora-start" ''
    last_nonempty_line=$(${hora-status}/bin/hora-status)
    (echo $last_nonempty_line | ${gnugrep}/bin/grep -q "^o") || {
        echo "Last activity must be closed: $last_nonempty_line" >/dev/stderr
        exit 1
    }

    account=$1
    (${hora}/bin/hora accounts | ${gnugrep}/bin/grep -q "^$account\$") || {
        echo "The account '$account' is not known. Please add manually."
        exit 1
    }

    message=$2
    date=$(${date})
    echo "i $date $account  $message\n" >> "${timeLedger}"
    echo "Started $account at $date" >/dev/stderr 
  '';

  hora-stop = writers.writeDashBin "hora-stop" ''
    last_nonempty_line=$(${hora-status}/bin/hora-status)

    (echo $last_nonempty_line | ${gnugrep}/bin/grep "^i") || {
        echo "Last activity cannot be closed: $last_nonempty_line" >/dev/stderr
        exit 1
    }

    last_activity=$(echo "$last_nonempty_line" | ${coreutils}/bin/cut -d' ' -f 4)
    date=$(${date})

    echo "o $date\n" >> ${timeLedger}
    echo "Stopped $last_activity at $date" >/dev/stderr
  '';

  hora-year = writers.writeDashBin "hora-year" ''
    ${hora}/bin/hora balance --tree --monthly --begin $(${coreutils}/bin/date +%Y) --depth 1
  '';
  hora-git = writers.writeDashBin "hora-git" ''
    directory=$(${coreutils}/bin/dirname ${lib.escapeShellArg timeLedger})
    if [ $# -gt 0 ]
    then
      ${git}/bin/git -C "$directory" --all --message=$(${date})
    else
      ${git}/bin/git -C "$directory" "$@"
    fi
  '';
  hora-weekly = writers.writeDashBin "hora-weekly" ''
    ${hora}/bin/hora register -p weekly --depth 1 --empty
  '';
in
symlinkJoin {
  name = "hora";
  paths = [
    hora
    hora-edit
    hora-start
    hora-status
    hora-stop
    hora-year
    hora-git
    hora-weekly
  ];
}
