{ pkgs, ... }:
{
  environment.variables.EDITOR = pkgs.lib.mkForce "nvim";
  environment.shellAliases.vi = "nvim";
  environment.shellAliases.vim = "nvim";
  environment.shellAliases.view = "nvim -R";

  nixpkgs.config.packageOverrides = pkgs: {
    vimPlugins = pkgs.vimPlugins // {
      cheat-sh-vim = pkgs.callPackage <niveum/packages/vimPlugins/cheat-sh.nix> {};
      vim-fetch = pkgs.callPackage <niveum/packages/vimPlugins/vim-fetch.nix> {};
      vim-colors-paramount = pkgs.callPackage <niveum/packages/vimPlugins/vim-colors-paramount.nix> {};
      vim-256noir = pkgs.callPackage <niveum/packages/vimPlugins/vim-256noir.nix> {};
      todo-txt-vim = pkgs.callPackage <niveum/packages/vimPlugins/todo-txt-vim.nix> {};
      jq-vim = pkgs.callPackage <niveum/packages/vimPlugins/jq-vim.nix> {};
      vim-fsharp = pkgs.callPackage <niveum/packages/vimPlugins/vim-fsharp.nix> {};
    };
  };

  environment.systemPackages = [
    (pkgs.neovim.override {
      configure = {
        customRC = builtins.readFile <niveum/dot/init.vim>;
        packages.nvim = with pkgs.vimPlugins; {
          start = [
            ale
            deoplete-nvim
            fzf-vim
            fzfWrapper
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
            cheat-sh-vim
            vim-fetch
            vim-colors-paramount
            vim-256noir
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
            purescript-vim
            vim-nix
            vim-toml
            vimtex
            vim-ledger
            todo-txt-vim
            emmet-vim
            jq-vim
            vim-fsharp
          ];
        };
      };
    })
  ];
}
