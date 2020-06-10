{ pkgs, ... }:
let
  nixify = pkgs.writers.writeDashBin "nixify" ''
    set -efuC

    if [ ! -e ./.envrc ]; then
      echo use_nix > .envrc
      direnv allow
    fi
    if [ ! -e shell.nix ]; then
      cat > shell.nix <<'EOF'
    { pkgs ? import <nixpkgs> {} }:
    pkgs.mkShell {
      buildInputs = with pkgs; [];
      shellHook = "export HISTFILE=''${toString ./.history}";
    }
    EOF
      ''${EDITOR:-vim} shell.nix
    fi
  '';
in {
  environment.systemPackages = [ pkgs.direnv nixify ];

  home-manager.users.me.programs.direnv = {
    enable = true;
    stdlib = builtins.readFile ("${
        pkgs.fetchFromGitHub {
          owner = "Mic92";
          repo = "dotfiles";
          rev = "a0a9b7e358fa70a85cd468f8ca1fbb02ae0a91df";
          sha256 = "1y9h5s1lf59sczsm0ksq2x1yhl98ba9lwk5yil3q53rg7n4574pg";
        }
      }/home/.direnvrc");
  };

  programs.zsh.interactiveShellInit = ''
    eval "$(${pkgs.direnv}/bin/direnv hook zsh)"
  '';
}
