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
      vim-reason-plus = pkgs.callPackage <niveum/packages/vimPlugins/vim-reason-plus.nix> {};
    };
  };

  environment.systemPackages = [
    (pkgs.neovim.override {
      configure = {
        customRC = builtins.readFile <niveum/dot/init.vim>;
        packages.nvim = with pkgs.vimPlugins; {
          start = [
            # cheat-sh-vim
            # deoplete-nvim
            # vim-abolish
            ale
            fzf-vim
            fzfWrapper
            goyo
            tabular
            vim-256noir
            vim-colors-paramount
            vim-commentary
            vim-eunuch
            vim-fetch
            vim-fugitive
            vim-gitgutter
            vim-pandoc vim-pandoc-syntax # vim-pandoc-after
            vim-repeat
            vim-sensible
            vim-startify
            vim-surround
          ];
          opt = [
            csv
            dhall-vim
            elm-vim
            emmet-vim
            haskell-vim
            idris-vim
            jq-vim
            purescript-vim
            rust-vim
            todo-txt-vim
            typescript-vim
            vim-fsharp
            vim-javascript
            vim-ledger
            vim-nix
            vim-reason-plus
            vim-toml
            vimtex
          ];
        };
      };
    })
  ];
}
