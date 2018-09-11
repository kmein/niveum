''
filetype off

" if exists("g:loaded_airline")
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
" endif
" if exists("g:loaded_airline_themes")
let g:airline_theme='simple'
" endif

" if exists("g:loaded_syntastic_plugin")
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
map ,s :SyntasticToggleMode<CR>
" endif

" if exists("g:loaded_gundo")
nnoremap <F5> :GundoToggle<CR>
" endif

" if supertab
let g:SuperTabDefaultCompletionType = '<c-n>'
if has("unix")
  inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
endif

" if tabular
vmap a= :Tabularize /=<CR>
vmap a; :Tabularize /::<CR>
vmap a- :Tabularize /-><CR>

colorscheme delek

set path=$PWD/**
set completeopt=menu,longest
set wildmode=longest,list,full
set shortmess+=aI
set nowritebackup noswapfile
set mouse=a
set showmatch
set encoding=utf8
set ffs=unix,dos,mac
set smartindent
set nowrap
set nohlsearch
set clipboard=unnamedplus,autoselect
set nopaste
set list listchars=tab:▸\ ,extends:❯,precedes:❮,nbsp:⍽ showbreak=↪
set foldlevelstart=30

" if exists("g:loaded_netrwPlugin")
let g:netrw_banner=0
let g:netrw_browse_split=4
let g:netrw_altv=1 " open splits to the right
let g:netrw_liststyle=3 " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'
" endif

call matchadd('colorcolumn', '\%101v', 100)
highlight folded ctermbg=black
highlight colorcolumn ctermbg=red
highlight matchparen cterm=bold ctermbg=black ctermfg=white
highlight TrailSpace ctermbg=red guibg=darkred
match TrailSpace /\s\+$/

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

nmap <C-j> ddp
nmap <C-k> ddkP
vmap <C-j> xp`[V`]
vmap <C-k> xkP`[V`]

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
  autocmd filetype markdown,text set formatprg=par\ -w80
  autocmd filetype haskell set formatprg=hindent
  autocmd filetype markdown,text set textwidth=80
  autocmd filetype markdown,text set formatoptions+=t
  autocmd bufreadpost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \ exe "normal! g`\"" |
    \ endif
  autocmd bufwritepre * :call <SID>StripTrailingWhitespaces()
  autocmd bufnewfile,bufread *.md set filetype=markdown.pandoc
  autocmd bufnewfile,bufread *.asm set filetype=nasm
  autocmd bufnewfile,bufread *.bf set filetype=brainfuck
  autocmd bufnewfile,bufread *.do set filetype=sh
  autocmd bufnewfile,bufread config set filetype=conf
  autocmd bufnewfile,bufread *.conf set filetype=conf
  autocmd bufnewfile,bufread *.4th set filetype=forth
  autocmd bufnewfile,bufread *.c set keywordprg=man\ 3
  autocmd bufnewfile,bufread *.h set keywordprg=man\ 3

  autocmd filetype make setlocal noexpandtab

  autocmd bufreadpre * setlocal foldmethod=indent
  autocmd bufwinenter * if &fdm == 'indent' | setlocal foldmethod=manual | endif
endif
''
