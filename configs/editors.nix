{ pkgs, config, ... }:
let vimConfig = import <dot/vim.nix> { inherit config pkgs; };
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
            # (pkgs.vimUtils.buildVimPlugin rec {
            #   name = "connermcd";
            #   src = pkgs.fetchFromGitHub {
            #     owner = "connermcd";
            #     repo = "dotfiles";
            #     rev = "3a2788cc94e5e51144adcad4da4f9489ccd3e341";
            #     sha256 = "1p0r4nd4syhy62mkz1iji6kwsg2hvcr7q5qzaqv6p52dkm7ffx52";
            #   };
            #   buildPhase = ''
            #     mkdir -p $out/share/vim-plugins/${name}/colors
            #     mv .vim/colors/*.vim $out/share/vim-plugins/${name}/colors/
            #   '';
            # })
            # (pkgs.vimUtils.buildVimPluginFrom2Nix {
            #   name = "apprentice";
            #   src = pkgs.fetchFromGitHub {
            #     owner = "romainl";
            #     repo = "Apprentice";
            #     rev = "0ca2038758f9d7dfdf51733db8d22665663382f7";
            #     sha256 = "1jdfn3wm46ndc24lkzxy3akjbqwglrdy7qqyypbwwsq7vp0s5051";
            #   };
            # })
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
