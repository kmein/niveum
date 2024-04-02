{
  config,
  pkgs,
  lib,
  inputs,
  niveumPackages,
  ...
}: let
  system = "x86_64-darwin";

  nextcloud = "${config.home.homeDirectory}/Nextcloud/ZODIAC";
  timeLedger = "${nextcloud}/hora.timeclock";

  adminEssentials = import ../../configs/admin-essentials.nix {
    inherit pkgs niveumPackages lib system;
  };

  stardict = import ../../configs/stardict.nix {
    inherit pkgs lib inputs;
  };

  git = import ../../configs/git.nix {
    inherit pkgs lib inputs system;
  };
in {
  home.packages =
    [
      (pkgs.writers.writeDashBin "hora" ''
        ${pkgs.hledger}/bin/hledger -f "${timeLedger}" "$@"
      '')
      (pkgs.writers.writeDashBin "hora-edit" ''
        nvim + "${timeLedger}"
      '')
      (pkgs.writers.writeDashBin "hora-status" ''
        ${pkgs.coreutils}/bin/tac "${timeLedger}" | ${pkgs.gnugrep}/bin/grep -m 1 . 
      '')
      (pkgs.writers.writeDashBin "hora-start" ''
        [ -w "${timeLedger}" ] || {
            echo "${timeLedger}" is not a writable file >/dev/stderr
            exit 1
        }
        last_nonempty_line=$(hora-status)
        (echo $last_nonempty_line | ${pkgs.gnugrep}/bin/grep -q "^o") || {
            echo "Last activity must be closed: $last_nonempty_line" >/dev/stderr
            exit 1
        }
        account=$1
        (hora accounts | ${pkgs.gnugrep}/bin/grep -q "^$account\$") || {
            echo "The account '$account' is not known. Please add manually."
            exit 1
        }

        message=$2
        date=$(${pkgs.coreutils}/bin/date +"%Y-%m-%d %H:%M:%S")
        echo "i $date $account  $message\n" >> "${timeLedger}"
        echo Started $account at $date >/dev/stderr 
      '')
      (pkgs.writers.writeDashBin "hora-stop" ''
        [ -w "${timeLedger}" ] || {
            echo "${timeLedger}" is not a writable file >/dev/stderr
            exit 1
        }
        last_nonempty_line=$(hora-status)
        (echo $last_nonempty_line | ${pkgs.gnugrep}/bin/grep "^i") || {
            echo "Last activity cannot be closed: $last_nonempty_line" >/dev/stderr
            exit 1
        }

        last_activity=$(${pkgs.coreutils}/bin/tail -n 1 ${timeLedger} | ${pkgs.coreutils}/bin/cut -d' ' -f 4)
        date=$(${pkgs.coreutils}/bin/date +"%Y-%m-%d %H:%M:%S")

        echo "o $date\n" >> ${timeLedger}
        echo Stopped $last_activity at $date >/dev/stderr
      '')
      niveumPackages.vim
      pkgs.ghc
      pkgs.python3
    ]
    ++ adminEssentials.environment.systemPackages
    ++ git.environment.systemPackages;
  #++ stardict.environment.systemPackages;

  home.shellAliases =
    adminEssentials.environment.shellAliases
    // git.environment.shellAliases;

  programs.git = git.home-manager.users.me.programs.git;

  programs.zsh = let
    promptColours = {
      success = "green";
      failure = "red";
    };
  in {
    autocd = true;
    defaultKeymap = "viins";
    enableAutosuggestions = true;
    enableCompletion = true;
    enable = true;
    historySubstringSearch.enable = true;
    syntaxHighlighting.enable = true;
    syntaxHighlighting.highlighters = ["main" "brackets" "pattern" "line"];
    initExtra = ''
      # ref https://gist.github.com/meeech/0b97a86f235d10bc4e2a1116eec38e7e 
      if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; 
      then
        . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
      fi

      precmd () {
        if [[ -n $IN_NIX_SHELL ]]; then
          PROMPT='%B%~%b %(?.%F{${promptColours.success}}.%F{${promptColours.failure}})λ%f '
        else
          PROMPT='%B%~%b %(?.%F{${promptColours.success}}.%F{${promptColours.failure}})%#%f '
        fi
        print -Pn "\e]2;%n@%M:%~\a" # title bar prompt
      }
      zle-keymap-select zle-line-init () {
        case $KEYMAP in
          vicmd) print -n '\e]12;green\a';;
          viins|main) print -n '\e]12;gray\a';;
        esac
      }
      zle -N zle-line-init
      zle -N zle-keymap-select
    '';
  };

  home.sessionVariables.EDITOR = "${niveumPackages.vim}/bin/nvim";
  home.file."Local Applications".source = pkgs.symlinkJoin {
    name = "local-applications";
    paths = [pkgs.anki-bin pkgs.dbeaver pkgs.vscode pkgs.stellarium];
  };
  home.stateVersion = "23.11";
  home.username = "xm7234fu";
  home.homeDirectory = "/Users/${config.home.username}";
  nixpkgs.config.allowUnfree = true;
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = "experimental-features = nix-command flakes";
}

/*
hora register -p weekly --depth 1 --empty


*/