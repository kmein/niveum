" if tabular
vnoremap a= :Tabularize /=<CR>
vnoremap a; :Tabularize /::<CR>
vnoremap a- :Tabularize /-><CR>

colorscheme dim

" noremap <Leader>h :<C-u>split<CR>
" noremap <Leader>v :<C-u>vsplit<CR>
noremap <Leader>gs :Git<CR>
noremap <Leader>gc :Git commit<CR>

" reindent whole file
noremap <leader>i mzgg=G`z

" replace all
nnoremap S :%s//g<Left><Left>

" Hit `%` on `if` to jump to `else`.
runtime macros/matchit.vim

set notitle
set nospell
set backspace=indent,eol,start
set hidden
set ruler
set shiftwidth=2 tabstop=2 expandtab
set laststatus=1
set number
set path+=**
set splitbelow splitright
set wildmenu wildmode=longest,list,full
set shortmess+=ac
set showmatch
set smartindent
set hlsearch
set foldlevelstart=30

nnoremap <C-H> <CMD>set nohlsearch<CR>

let g:netrw_banner=0
let g:netrw_browse_split=4
let g:netrw_altv=1 " open splits to the right
let g:netrw_liststyle=3 " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'
let g:netrw_winsize = 25

call matchadd('colorcolumn', '\%101v', 100)
highlight colorcolumn ctermbg=red

" undofile - This allows you to use undos after exiting and restarting
" This, like swap and backups, uses .vim-undo first, then ~/.vim/undo
" :help undo-persistence
if exists("+undofile")
  if isdirectory($HOME . '/.vim/undo') == 0
    :silent !mkdir -p ~/.vim/undo > /dev/null 2>&1
  endif
  set undodir=~/.vim/undo/
  set undofile
endif

nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
vnoremap <Space> zf

command! RandomLine execute 'normal! '.(system('/bin/sh -c "echo -n $RANDOM"') % line('$')).'G'

function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  execute "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
command! DiffSaved call s:DiffWithSaved()

" BACKGROUND COLOR TOGGLE
function! s:toggle_background()
  if &background ==# "light"
    set background=dark
  elseif &background ==# "dark"
    set background=light
  endif
endfunction
command! ToggleBackground call s:toggle_background()
inoremap <F12> <C-O>:ToggleBackground<CR>
nnoremap <F12> :ToggleBackground<CR>

augroup filetypes
  autocmd!
  autocmd bufnewfile,bufread *.4th set filetype=forth
  autocmd bufnewfile,bufread *.asm set filetype=nasm
  autocmd bufnewfile,bufread *.c set keywordprg=man\ 3
  autocmd bufnewfile,bufread *.h set keywordprg=man\ 3
  autocmd bufnewfile,bufread *.conf set filetype=conf
  autocmd bufnewfile,bufread *.nix packadd vim-nix | set filetype=nix | set path+=/var/src
  autocmd bufnewfile,bufread *.rust packadd rust-vim
  autocmd bufnewfile,bufread *.csv packadd csv.vim | set filetype=csv
  autocmd bufnewfile,bufread *.tex packadd vimtex | set filetype=tex
  autocmd bufnewfile,bufread *.typ packadd typst.vim | set filetype=typst
  autocmd bufnewfile,bufread *.ics packadd icalendar.vim | set filetype=icalendar
  autocmd bufnewfile,bufread *.ts packadd typescript-vim | set filetype=typescript
  autocmd bufnewfile,bufread *.jq packadd jq.vim | set filetype=jq
  autocmd bufnewfile,bufread *.journal packadd vim-ledger | set filetype=ledger shiftwidth=4
  autocmd bufnewfile,bufread urls,config set filetype=conf
  autocmd bufnewfile,bufread *.elm packadd elm-vim | set filetype=elm shiftwidth=4
  autocmd bufnewfile,bufread *.md packadd vim-pandoc | packadd vim-pandoc-syntax | set filetype=pandoc
  autocmd filetype haskell packadd haskell-vim | set keywordprg=hoogle\ -i
  autocmd filetype javascript packadd vim-javascript
  autocmd filetype make setlocal noexpandtab
  autocmd filetype html packadd emmet-vim
  autocmd filetype gitcommit setlocal spell spelllang=en
  autocmd filetype mail setlocal spell spelllang=de textwidth=0
augroup end


autocmd bufreadpost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \ exe "normal! g`\"" |
      \ endif
autocmd bufreadpre * setlocal foldmethod=indent

set completeopt=menu,menuone,noselect
set complete+=kspell

let g:pandoc#syntax#conceal#use = 0
let g:pandoc#modules#disabled = []
let g:pandoc#spell#default_langs = ['en', 'de']
