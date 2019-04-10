{ pkgs, config, ... }:
let vimConfig = import <niveum/dot/vim.nix> { inherit config pkgs; };
in {
  programs.nano.nanorc = ''
    set autoindent
    set boldtext
    set morespace
    set smarthome
    set tabsize 4
    set tabstospaces
  '';

  environment.variables.EDITOR = pkgs.lib.mkForce "nvim";
  environment.shellAliases.vi = "nvim";
  environment.shellAliases.vim = "nvim";
  environment.shellAliases.view = "nvim -R";
  environment.systemPackages = [pkgs.nvim];

  nixpkgs.config.packageOverrides = pkgs: {
    nvim = pkgs.neovim.override {
      configure = {
        customRC = builtins.readFile <niveum/dot/vimrc>;
        packages.nvim = with pkgs.vimPlugins; {
          start = with pkgs.vimPlugins; [
            ale
            deoplete-nvim
            fzf-vim
            fzfWrapper
            supertab
            tabular
            vim-abolish
            vim-airline vim-airline-themes
            vim-commentary
            vim-eunuch
            vim-fugitive
            vim-gitgutter
            vim-pandoc vim-pandoc-after vim-pandoc-syntax
            vim-repeat
            vim-sensible
            vim-startify
            vim-surround
          ];
          opt = [
            csv
            deoplete-rust
            dhall-vim
            elm-vim
            haskell-vim
            idris-vim
            rust-vim
            typescript-vim
            vim-javascript
            vim-nix
            vimtex
            vim-ledger
            (pkgs.vimUtils.buildVimPluginFrom2Nix {
              name = "vim-fsharp";
              src = pkgs.fetchFromGitHub {
                owner = "fsharp";
                repo = "vim-fsharp";
                rev = "627db7d701747e8fd7924b6505c61e16a369fb72";
                sha256 = "00hhgn2p54faysx1ddccyhl9jnvddgxsczhv0np3mgzza6ls4838";
              };
            })
            (pkgs.vimUtils.buildVimPluginFrom2Nix {
              name = "emmet-vim";
              src = pkgs.fetchFromGitHub {
                owner = "mattn";
                repo = "emmet-vim";
                rev = "d698f1658770ca5fa58c87e80421c8d65bbe9065";
                sha256 = "0vl4267hh8g1vkvc3awlqyypgz4m1r43d47sldl80yamiafiviaj";
              };
            })
          ];
        };
      };
    };
  };
}
