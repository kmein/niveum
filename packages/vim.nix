{
  neovim,
  vimPlugins,
  fetchFromGitHub,
  vimUtils,
  niveumPackages,
  ...
}: (neovim.override {
  configure = {
    customRC = ''
      source ${../lib/vim/shared.vim}
      source ${../lib/vim/init.vim}
      let g:snippet_directory = '${vimPlugins.friendly-snippets}'
      luafile ${../lib/vim/init.lua}
    '';
    packages.nvim = with vimPlugins; {
      start = [
        nvim-cmp
        cmp-buffer
        cmp-path
        cmp-nvim-lsp
        cmp-cmdline
        luasnip

        editorconfig-vim

        fzf-vim
        fzfWrapper
        supertab
        undotree
        tabular
        # vimwiki
        niveumPackages.vimPlugins-vim-colors-paramount
        nvim-lspconfig
        vim-commentary
        vim-css-color
        vim-eunuch
        niveumPackages.vimPlugins-vim-fetch
        vim-fugitive
        vim-gitgutter
        vim-repeat
        vim-sensible
        vim-surround
        (vimUtils.buildVimPlugin rec {
          pname = "vim-dim";
          version = "1.1.0";
          name = "${pname}-${version}";
          src = fetchFromGitHub {
            owner = "jeffkreeftmeijer";
            repo = pname;
            rev = version;
            sha256 = "sha256-lyTZUgqUEEJRrzGo1FD8/t8KBioPrtB3MmGvPeEVI/g=";
          };
        })
      ];
      opt = [
        csv
        dhall-vim
        elm-vim
        emmet-vim
        haskell-vim
        niveumPackages.vimPlugins-icalendar-vim
        niveumPackages.vimPlugins-jq-vim
        rust-vim
        typescript-vim
        vim-javascript
        vim-ledger
        vim-nix
        vimtex
        vim-pandoc
        vim-pandoc-syntax
        niveumPackages.vimPlugins-vim-256noir
        niveumPackages.vimPlugins-typst-vim
      ];
    };
  };
})
