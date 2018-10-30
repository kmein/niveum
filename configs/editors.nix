{ pkgs, config, ... }:
with import ../helpers.nix;
let vim_conf = ''
  " if tabular
  vmap a= :Tabularize /=<CR>
  vmap a; :Tabularize /::<CR>
  vmap a- :Tabularize /-><CR>

  colorscheme delek

  filetype plugin indent on
  set nocompatible
  set smartcase
  set shiftwidth=2 tabstop=2 expandtab
  set number
  set path=$PWD/**
  set completeopt=menu,longest
  set wildmode=list:longest wildignore+=${commaSep config.constants.ignore}
  set shortmess+=aI
  set nowritebackup noswapfile
  set mouse=a
  set showmatch
  set encoding=utf8 ffs=unix,dos,mac
  set smartindent
  set nowrap
  set nohlsearch
  set clipboard=unnamedplus,autoselect
  set nopaste
  set list listchars=tab:▸\ ,extends:❯,precedes:❮,nbsp:⍽,trail:· showbreak=↪
  set foldlevelstart=30

  if exists("g:loaded_netrwPlugin")
    let g:netrw_banner=0
    let g:netrw_browse_split=4
    let g:netrw_altv=1 " open splits to the right
    let g:netrw_liststyle=3 " tree view
    let g:netrw_list_hide=netrw_gitignore#Hide()
    let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'
  endif

  call matchadd('colorcolumn', '\%101v', 100)
  highlight folded ctermbg=black
  highlight colorcolumn ctermbg=red
  highlight matchparen cterm=bold

  " undofile - This allows you to use undos after exiting and restarting
  " This, like swap and backups, uses .vim-undo first, then ~/.vim/undo
  " :help undo-persistence
  if exists("+undofile")
    if isdirectory($HOME . '/.vim/undo') == 0
      :silent !mkdir -p ~/.vim/undo > /dev/null 2>&1
    endif
    set undodir=./.vim-undo//
    set undodir+=~/.vim/undo//
    set undofile
  endif

  nmap <C-j> ddp | vmap <C-j> xp`[V`]
  nmap <C-k> ddkP | vmap <C-k> xkP`[V`]

  nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
  vnoremap <Space> zf

  command! RandomLine execute 'normal! '.(system('/bin/bash -c "echo -n $RANDOM"') % line('$')).'G'

  function! <SID>StripTrailingWhitespaces()
    let _s=@/
    let l=line(".")
    let c=col(".")

    %s/\s\+$//e

    let @/=_s
    call cursor(l,c)
  endfunction

  function! s:DiffWithSaved()
    let filetype=&ft
    diffthis
    vnew | r # | normal! 1Gdd
    diffthis
    execute "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
  endfunction
  command! DiffSaved call s:DiffWithSaved()

  if has("autocmd")
    autocmd bufnewfile,bufread *.4th set filetype=forth
    autocmd bufnewfile,bufread *.asm set filetype=nasm
    autocmd bufnewfile,bufread *.c set keywordprg=man\ 3
    autocmd bufnewfile,bufread *.conf set filetype=conf
    autocmd bufnewfile,bufread *.do set filetype=sh
    autocmd bufnewfile,bufread *.fs :packadd vim-fsharp | set filetype=fsharp
    autocmd bufnewfile,bufread *.h set keywordprg=man\ 3
    autocmd bufnewfile,bufread *.md set filetype=markdown.pandoc
    autocmd bufnewfile,bufread *.nix :packadd vim-nix
    autocmd bufnewfile,bufread *.rust :packadd rust-vim deoplete-rust
    autocmd bufnewfile,bufread *.tex :packadd vimtex | set filetype=tex
    autocmd bufnewfile,bufread *.ts :packadd vim-typescript
    autocmd bufnewfile,bufread *.graphql :packadd vim-graphql
    autocmd bufnewfile,bufread config set filetype=conf
    autocmd filetype haskell :packadd Hoogle
    autocmd filetype haskell set formatprg=hindent
    autocmd filetype python set formatprg=black
    autocmd filetype javascript *.js :packadd vim-javascript
    autocmd filetype make setlocal noexpandtab
    autocmd filetype markdown,text set formatoptions+=t
    autocmd filetype markdown,text set formatprg=par\ -w80
    autocmd filetype markdown,text set textwidth=80

    autocmd bufreadpost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \ exe "normal! g`\"" |
      \ endif
    autocmd bufreadpre * setlocal foldmethod=indent
    autocmd bufwritepre * :call <SID>StripTrailingWhitespaces()
    autocmd bufwinenter * if &fdm == 'indent' | setlocal foldmethod=manual | endif
  endif

  "if exists("g:loaded_airline")
    set noshowmode laststatus=0 noruler
    let g:airline#extensions#tabline#close_symbol = 'X'
    let g:airline#extensions#tabline#enabled = 0
    let g:airline#extensions#tabline#left_alt_sep = ''
    let g:airline#extensions#tabline#left_sep = ''
    let g:airline#extensions#tabline#right_alt_sep = ''
    let g:airline#extensions#tabline#right_sep = ''
    let g:airline#extensions#tabline#show_close_button = 1
    let g:airline#extensions#tabline#show_tab_type = 0
    let g:airline#extensions#tabline#tab_min_count = 2
    let g:airline#extensions#tabline#tab_nr_type = 0
    let g:airline#extensions#tmuxline#enabled = 0
    let g:airline#extensions#wordcount#enabled = 1
    let g:airline_left_alt_sep = ''
    let g:airline_left_sep = ''
    let g:airline_right_alt_sep = ''
    let g:airline_right_sep = ''
    let g:airline_section_z = '%{line(".")}/%{line("$")} %{col(".")}'
  "endif
  "if exists("g:loaded_airline_themes")
    let g:airline_theme='simple'
  "endif
'';
in {
  environment.variables.EDITOR = pkgs.lib.mkForce "vim";
  environment.shellAliases.vi = "vim";
  environment.shellAliases.view = "vim -R";

  programs.nano.nanorc = ''
    set autoindent
    set boldtext
    set morespace
    set smarthome
    set tabsize 4
    set tabstospaces
  '';

  environment.systemPackages = [(
    with import <nixpkgs> {};
    pkgs.vim_configurable.customize {
      name = "vim";
      vimrcConfig.customRC = vim_conf;
      vimrcConfig.packages.vim = with pkgs.vimPlugins; {
        start = [
          ctrlp
          deoplete-nvim
          supertab
          syntastic
          tabular
          vim-airline vim-airline-themes
          vim-commentary
          vim-eunuch
          vim-fugitive
          vim-gitgutter
          vim-pandoc vim-pandoc-after vim-pandoc-syntax
          vim-repeat
          vim-sensible
          vim-startify
          vim-surround
        ];
        opt = [
          Hoogle
          deoplete-rust
          idris-vim
          vimtex
          rust-vim
          typescript-vim
          vim-javascript
          vim-nix
          (vimUtils.buildVimPluginFrom2Nix {
            name = "vim-graphql-2018-09-29";
            src = fetchFromGitHub {
              owner = "jparise";
              repo = "vim-graphql";
              rev = "6a15d21b74bbb3d7ee30b5575ef5c4171fe999ba";
              sha256 = "03l5yj77cgpvq16d59g6mrgacs9rps0ppbaipj5klbp7bi6n02gi";
            };
          })
          (vimUtils.buildVimPluginFrom2Nix {
            name = "vim-fsharp-2018-04-19";
            src = fetchFromGitHub {
              owner = "fsharp";
              repo = "vim-fsharp";
              rev = "627db7d701747e8fd7924b6505c61e16a369fb72";
              sha256 = "00hhgn2p54faysx1ddccyhl9jnvddgxsczhv0np3mgzza6ls4838";
            };
          })
        ];
      };
    }
  )];
}
