" http://nvie.com/posts/how-i-boosted-my-vim/
" http://stevelosh.com/blog/2010/09/coming-home-to-vim/
" http://vim.runpaint.org/toc/
" http://vim.wikia.com/wiki/In_line_copy_and_paste_to_system_clipboard
" http://vimcasts.org/episodes/archive
" http://vimdoc.sourceforge.net/htmldoc/map.html#:map-modes
" http://vimdoc.sourceforge.net/htmldoc/usr_41.html
" http://www.vim.org/scripts/script.php?script_id=2340

" set leader, nocompat, color, and syntax
let mapleader=","
set nocompatible
set modelines=0 " disable due to a potential security hole.
filetype plugin indent on
set t_Co=256
colorscheme molokai
syntax on

" set basic features
set autoread
set encoding=utf-8
set paste
set pastetoggle=<F2>
set title showmode showcmd ruler
set laststatus=2 " always (default: only if there are 2+ windows)
set visualbell noerrorbells hidden
set nocursorcolumn nocursorline
" set mouse=a " a for gui (default: '')
set scrolloff=3
set ttyfast
set guioptions-=T
set guifont=Menlo:h12

" set tabbing and wrapping
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab smarttab
set autoindent copyindent shiftround nosmartindent
set backspace=indent,eol,start
set textwidth=79
set formatoptions=qrn1 " for gq http://vimdoc.sourceforge.net/htmldoc/change.html#fo-table
set listchars=tab:▸\ ,eol:¬ " tab:▸\ ,trail:.,extends:#,nbsp:.
set wrap showbreak=…
nnoremap <leader>r :set invwrap wrap?<CR>
nnoremap <leader>l :set list!<CR>
nnoremap <leader>L :%s=\s\+$==<CR>

" search w/ very nomagic '\V'
set nowrapscan ignorecase smartcase incsearch hlsearch gdefault
set showmatch
nnoremap <silent> <space> :nohlsearch<bar>:echo<CR>
noremap / /\V
noremap ? ?\V

" move around
nnoremap j gj
nnoremap k gk
nnoremap ^ g^
nnoremap $ g$

" escape quicker
inoremap jj <Esc>
inoremap ;; <Esc>:

" break (shift-K), opposite of join (shift-J)
noremap K i<CR><Esc>

" miscellaneous
set nobackup nowritebackup noswapfile " they are 70's ways
set history=200 undolevels=1000
set number

" jump to the last edited line based on .viminfo
autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g'\"" | endif
