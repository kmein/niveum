{
  neovim,
  vimPlugins,
  obsidiantVaultDirectory ? "~/cloud/syncthing/obsidian/",
  ...
}:
neovim.override {
  configure = {
    customRC = ''
      source ${../lib/vim/shared.vim}

      cd ${obsidiantVaultDirectory}

      let g:vimwiki_list = [{'path': '${obsidiantVaultDirectory}',
        \ 'syntax': 'markdown', 'ext': '.md'}]

      let NERDTreeSortOrder = ['[[-timestamp]]']

      " Start NERDTree and put the cursor back in the other window.
      autocmd VimEnter * NERDTree ${obsidiantVaultDirectory} | wincmd p
    '';
    packages.nvim.start = [
      vimPlugins.vimwiki
      vimPlugins.nerdtree
      vimPlugins.fzf-vim
      vimPlugins.fzfWrapper
    ];
  };
}
