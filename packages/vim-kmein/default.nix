{
  neovim,
  vimPlugins,
  fetchFromGitHub,
  vimUtils,
  writeText,
  stylixColors ? null,
  colorscheme ? null,
  lib,
  ...
}: neovim.override {
  configure = {
    vimAlias = true;
    viAlias = true;
    customRC = ''
      source ${./shared.vim}
      source ${./init.vim}
      let g:snippet_directory = '${vimPlugins.friendly-snippets}'
      luafile ${./init.lua}
    '' + lib.optionalString (stylixColors != null) (with stylixColors.withHashtag; ''
      luafile ${writeText "colors.lua" ''
        require('base16-colorscheme').setup({
          base00 = '${base00}', base01 = '${base01}', base02 = '${base02}', base03 = '${base03}',
          base04 = '${base04}', base05 = '${base05}', base06 = '${base06}', base07 = '${base07}',
          base08 = '${base08}', base09 = '${base09}', base0A = '${base0A}', base0B = '${base0B}',
          base0C = '${base0C}', base0D = '${base0D}', base0E = '${base0E}', base0F = '${base0F}'
        })
      ''}
    '') + lib.optionalString (colorscheme != null) ''
      colorscheme ${colorscheme}
    '';
    packages.nvim = with vimPlugins; {
      start = [
        base16-nvim
        nvim-cmp
        cmp-buffer
        cmp-path
        cmp-nvim-lsp
        cmp-cmdline
        luasnip

        editorconfig-vim

        copilot-vim

        fzf-vim
        fzfWrapper
        supertab
        undotree
        tabular
        # vimwiki
        vimPlugins.vim-colors-paramount
        nvim-lspconfig
        vim-commentary
        vim-css-color
        vim-eunuch
        vimPlugins.vim-fetch
        vim-fugitive
        vim-gitgutter
        vim-repeat
        vim-sensible
        vim-surround
        (let version = "1.1.0"; pname = "vim-dim"; in vimUtils.buildVimPlugin {
          pname = "vim-dim";
          version = version;
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
        vim-elixir
        haskell-vim
        vimPlugins.icalendar-vim
        vimPlugins.jq-vim
        rust-vim
        typescript-vim
        vim-javascript
        vim-ledger
        vim-nix
        vimtex
        vim-pandoc
        vim-pandoc-syntax
        vimPlugins.vim-256noir
        vimPlugins.typst-vim
      ];
    };
  };
}
