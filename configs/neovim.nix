{pkgs, ...}: {
  environment.variables.EDITOR = pkgs.lib.mkForce "nvim";
  environment.shellAliases.vi = "nvim";
  environment.shellAliases.vim = "nvim";
  environment.shellAliases.view = "nvim -R";

  nixpkgs.config.packageOverrides = pkgs: {
    vimPlugins =
      pkgs.vimPlugins
      // {
        cheat-sh-vim = pkgs.callPackage <niveum/packages/vimPlugins/cheat-sh.nix> {};
        vim-fetch = pkgs.callPackage <niveum/packages/vimPlugins/vim-fetch.nix> {};
        vim-colors-paramount = pkgs.callPackage <niveum/packages/vimPlugins/vim-colors-paramount.nix> {};
        vim-256noir = pkgs.callPackage <niveum/packages/vimPlugins/vim-256noir.nix> {};
        icalendar-vim = pkgs.callPackage <niveum/packages/vimPlugins/icalendar-vim.nix> {};
        jq-vim = pkgs.callPackage <niveum/packages/vimPlugins/jq-vim.nix> {};
        vim-fsharp = pkgs.callPackage <niveum/packages/vimPlugins/vim-fsharp.nix> {};
        vim-reason-plus = pkgs.callPackage <niveum/packages/vimPlugins/vim-reason-plus.nix> {};
        vim-mail = pkgs.callPackage <niveum/packages/vimPlugins/vim-mail.nix> {};
      };
  };

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "vim" ''neovim "$@"'')
    (pkgs.neovim.override {
      configure = {
        customRC = builtins.readFile <niveum/lib/vim/init.vim>;
        packages.nvim = with pkgs.vimPlugins; {
          start = [
            ale
            fzf-vim
            fzfWrapper
            supertab
            undotree
            tabular
            # vimwiki
            vim-colors-paramount
            vim-commentary
            vim-css-color
            vim-eunuch
            vim-fetch
            vim-fugitive
            vim-gitgutter
            vim-repeat
            vim-sensible
            vim-surround
            (pkgs.vimUtils.buildVimPlugin rec {
              pname = "vim-dim";
              version = "1.1.0";
              name = "${pname}-${version}";
              src = pkgs.fetchFromGitHub {
                owner = "jeffkreeftmeijer";
                repo = pname;
                rev = version;
                sha256 = "sha256-lyTZUgqUEEJRrzGo1FD8/t8KBioPrtB3MmGvPeEVI/g=";
              };
            })
          ];
          opt = [
            csv
            elm-vim
            emmet-vim
            haskell-vim
            icalendar-vim
            jq-vim
            rust-vim
            typescript-vim
            vim-javascript
            vim-ledger
            vim-nix
            vimtex
            vim-pandoc
            vim-pandoc-syntax
            vim-256noir
          ];
        };
      };
    })
  ];
}
