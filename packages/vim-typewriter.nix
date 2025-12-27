{
  neovim,
  vimPlugins,
  writers,
  wmctrl,
  ...
}:
let
  vim-typewriter = neovim.override {
    configure = {
      customRC = ''
        source ${./vim-kmein/shared.vim}

        function! s:goyo_enter()
          let b:quitting = 0
          let b:quitting_bang = 0
          autocmd QuitPre <buffer> let b:quitting = 1
          cabbrev <buffer> q! let b:quitting_bang = 1 <bar> q!
          Limelight
        endfunction

        function! s:goyo_leave()
          Limelight!
          " Quit Vim if this is the only remaining buffer
          if b:quitting && len(filter(range(1, bufnr('$')), 'buflisted(v:val)')) == 1
            if b:quitting_bang
              qa!
            else
              qa
            endif
          endif
        endfunction


        let g:limelight_conceal_ctermfg = 'gray'
        let g:limelight_conceal_guifg = 'DarkGray'
        let g:limelight_default_coefficient = 0.5
        let g:limelight_paragraph_span = 0


        autocmd! User GoyoEnter call <SID>goyo_enter()
        autocmd! User GoyoLeave call <SID>goyo_leave()
        autocmd VimEnter * Goyo
      '';
      packages.nvim.start = [
        vimPlugins.goyo
        vimPlugins.limelight-vim
        vimPlugins.mdwa-nvim
        vimPlugins.vim-ernest
      ];
    };
  };
in
writers.writeDashBin "vim-typewriter" ''
  # tell the window manager to fullscreen the nvim window
  ${wmctrl}/bin/wmctrl -r :ACTIVE: -b add,fullscreen
  ${vim-typewriter}/bin/nvim "$@
"''
