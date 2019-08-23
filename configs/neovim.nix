{ pkgs, ... }:
{
  environment.variables.EDITOR = pkgs.lib.mkForce "nvim";
  environment.shellAliases.vi = "nvim";
  environment.shellAliases.vim = "nvim";
  environment.shellAliases.view = "nvim -R";

  nixpkgs.config.packageOverrides = pkgs: {
    vimPlugins = pkgs.vimPlugins // {
      cheat-sh-vim = pkgs.callPackage <packages/vimPlugins/cheat-sh.nix> {};
      vim-fetch = pkgs.callPackage <packages/vimPlugins/vim-fetch.nix> {};
      vim-colors-paramount = pkgs.callPackage <packages/vimPlugins/vim-colors-paramount.nix> {};
      vim-256noir = pkgs.callPackage <packages/vimPlugins/vim-256noir.nix> {};
      todo-txt-vim = pkgs.callPackage <packages/vimPlugins/todo-txt-vim.nix> {};
      jq-vim = pkgs.callPackage <packages/vimPlugins/jq-vim.nix> {};
      vim-fsharp = pkgs.callPackage <packages/vimPlugins/vim-fsharp.nix> {};
    };
  };

  environment.systemPackages = [
    (pkgs.neovim.override {
      configure = {
        customRC = builtins.readFile <dot/init.vim>;
        packages.nvim = with pkgs.vimPlugins; {
          start = [
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
