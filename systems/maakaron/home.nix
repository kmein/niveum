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

  hora = pkgs.callPackage ../../packages/hora.nix {
    timeLedger = "${nextcloud}/hora.timeclock";
  };

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
      hora
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
  nix.package = pkgs.nixVersions.stable;
  nix.extraOptions = "experimental-features = nix-command flakes";
}

/*
hora register -p weekly --depth 1 --empty


*/
