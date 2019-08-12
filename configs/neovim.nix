{ pkgs, ... }:
{
  environment.variables.EDITOR = pkgs.lib.mkForce "nvim";
  environment.shellAliases.vi = "nvim";
  environment.shellAliases.vim = "nvim";
  environment.shellAliases.view = "nvim -R";

  environment.systemPackages = [
    (pkgs.neovim.override {
      configure = {
        customRC = builtins.readFile <dot/vimrc>;
        packages.nvim = with pkgs.vimPlugins; {
          start = with pkgs.vimPlugins; [
            ale
            deoplete-nvim
            fzf-vim
            fzfWrapper
            supertab
            tabular
            vim-abolish
            vim-commentary
            vim-eunuch
            vim-fugitive
            vim-gitgutter
            vim-pandoc vim-pandoc-after vim-pandoc-syntax
            vim-repeat
            vim-sensible
            vim-startify
            vim-surround
            (pkgs.vimUtils.buildVimPluginFrom2Nix rec {
              name = "vim-fetch";
              src = pkgs.fetchFromGitHub {
                owner = "wsdjeg";
                repo = "vim-fetch";
                rev = "76c08586e15e42055c9c21321d9fca0677442ecc";
                sha256 = "0avcqjcqvxgj00r477ps54rjrwvmk5ygqm3qrzghbj9m1gpyp2kz";
              };
            })
            (pkgs.vimUtils.buildVimPluginFrom2Nix rec {
              name = "vim-colors-paramount";
              src = pkgs.fetchFromGitHub {
                owner = "owickstrom";
                repo = "vim-colors-paramount";
                rev = "a5601d36fb6932e8d1a6f8b37b179a99b1456798";
                sha256 = "0rjn9vjb0xrxbiqyfclda2ridcbl3nfn4svs32mvmv8als6crncg";
              };
            })
            (pkgs.vimUtils.buildVimPluginFrom2Nix {
              name = "vim-256noir";
              src = pkgs.fetchFromGitHub {
                owner = "andreasvc";
                repo = "vim-256noir";
                rev = "e8668a18a4a90272c1cae87e655f8bddc5ac3665";
                sha256 = "1kpn379f5dgbsgb73g6d1nlmz9vz0j3ihi500mcdx4yg56fvkr0x";
              };
            })
          ];
          opt = [
            csv
            dhall-vim
            elm-vim
            haskell-vim
            idris-vim
            rust-vim
            typescript-vim
            vim-javascript
            vim-nix
            vim-toml
            vimtex
            vim-ledger
            (pkgs.vimUtils.buildVimPluginFrom2Nix {
              name = "jq.vim";
              src = pkgs.fetchFromGitHub {
                owner = "vito-c";
                repo = "jq.vim";
                rev = "5baf8ed192cf267d30b84e3243d9aab3d4912e60";
                sha256 = "1ykaxlli7b9wmhr8lpdalqxh7l4940jwhwm9pwlraga425h4r6z4";
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
          ];
        };
      };
    })
  ];
}
