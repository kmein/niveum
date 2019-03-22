{ pkgs, config, ... }:
let vimConfig = import ../dot/vim.nix { inherit config pkgs; };
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
        customRC = builtins.readFile ../dot/vimrc;
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
            deoplete-jedi
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
            (pkgs.vimUtils.buildVimPluginFrom2Nix {
              name = "vim-ledger";
              src = pkgs.fetchFromGitHub {
                owner = "ledger";
                repo = "vim-ledger";
                rev = "6eb3bb21aa979cc295d0480b2179938c12b33d0d";
                sha256 = "0rbwyaanvl2bqk8xm4kq8fkv8y92lpf9xx5n8gw54iij7xxhnj01";
              };
            })
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
            (pkgs.vimUtils.buildVimPluginFrom2Nix {
              name = "ghcid";
              src = "${(pkgs.fetchFromGitHub {
                owner = "ndmitchell";
                repo = "ghcid";
                rev = "5288801e7f046c42972527cd94171ce893ba91cf";
                sha256 = "0dgxsl1dci6w3x662c7z1zw8yvfnch4ymhsvx29n7jkgqmcy1lki";
              }).out}/plugins/nvim";
            })

          ];
        };
      };
    };
  };
}
