nnoremap <C-p> :FZF<CR>
nnoremap <C-l> :Rg<CR>
let g:fzf_layout = { 'down': '~15%' }

" transparent background
hi Normal guibg=NONE ctermbg=NONE

let mapleader = ","
let maplocalleader="\\"
noremap <leader>n :bn<CR>
noremap <leader>p :bp<CR>
noremap <leader>c :bd<CR>
noremap <leader>b :Buffers<CR>
noremap <leader>t :Tags<CR>

filetype plugin indent on
set autoindent
set smartcase ignorecase " you need these two
set nowritebackup noswapfile
set mouse=a
set encoding=utf8 ffs=unix,dos,mac
set wrap
set list listchars=tab:⇥\ ,extends:❯,precedes:❮,nbsp:␣,trail:· showbreak=¬
set clipboard=unnamedplus
set nopaste

iabbrev ddate <C-R>=strftime("%F")<CR>
iabbrev dtime <C-R>=strftime("%F %T")<CR>
