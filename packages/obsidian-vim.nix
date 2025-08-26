{
  neovim,
  vimPlugins,
  obsidiantVaultDirectory ? "~/state/obsidian/",
  ...
}:
neovim.override {
  configure = {
    customRC = ''
      source ${../lib/vim/shared.vim}

      cd ${obsidiantVaultDirectory}

      let g:vimwiki_auto_chdir = 1
      let g:vimwiki_listsyms = ' X'
      let g:vimwiki_commentstring = '<!--%s-->'

      let mapleader = ","
      nmap <Leader>r :NERDTreeFocus<cr>R<c-w><c-p>

      let g:vimwiki_list = [{
        \ 'path': '${obsidiantVaultDirectory}',
        \ 'syntax': 'markdown',
        \ 'ext': '.md',
        \ 'diary_rel_path' '.',
        \}]

      let NERDTreeSortOrder = ['[[-timestamp]]']

      " Start NERDTree and put the cursor back in the other window.
      autocmd VimEnter * NERDTree ${obsidiantVaultDirectory} | wincmd p
    '';
    packages.nvim.start = [
      vimPlugins.vimwiki
      vimPlugins.nerdtree
      vimPlugins.fzf-vim
      vimPlugins.fzfWrapper
      vimPlugins.vim-fugitive
    ];
  };
}
