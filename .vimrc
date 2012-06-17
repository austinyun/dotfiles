syntax on
set t_Co=256
set nocompatible

filetype plugin indent on
syntax enable

if has('win32') || has('win64')
    set lines=50
    set columns=80
    winpos 0 0 
    set guifont=Consolas:h10:cANSI
    source $VIMRUNTIME/mswin.vim
    behave mswin
    cd ~
endif

" o------------o
" | Backup     |
" o------------o
set nobackup
set nowb
set noswapfile

" o------------o
" | UI         |
" o------------o
set ruler
set nu
set laststatus=2
set showcmd

set showmatch
set matchtime=4

if has("gui_running")
    set guioptions=
    set guifont=Dina\ 10
    set cursorline
else
    set t_Co=256
endif

" o------------o
" | Behaviours |
" o------------o
set autoread
set autowrite
set autochdir
set wildmenu
set history=256

set ignorecase
set smartcase
set incsearch
set hlsearch

" o-------------o
" | Text Format |
" o-------------o
set smarttab
set expandtab
set autoindent
set tabstop=4
set shiftwidth=4
set backspace=2

" o-----------o
" |   Vundle  |
" o-----------o
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'altercation/vim-colors-solarized'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/nerdcommenter'
Bundle 'mutewinter/vim-indent-guides'
Bundle 'msanders/snipmate.vim'
Bundle 'AutoClose'
Bundle 'kien/ctrlp.vim'
Bundle 'tpope/vim-fugitive'

" o-----------------o
" | Plugin settings |
" o-----------------o
" NERDTree
nnoremap <left> :NERDTreeToggle<CR>
let NERDTreeShowBookmarks=1
let NERDTreeChDirMode=2 " Change the NERDTree directory to the root node
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" Indent Guides
if has("gui_running")
    let g:indent_guides_auto_colors=1
    let g:indent_guides_enable_on_vim_startup=1
    let g:indent_guides_guide_size=4
    let g:indent_guides_color_change_percent=5
endif


" o--------------o
" | Key mappings |
" o--------------o
map <up> :bn<cr>
map <down> :bp<cr>
"Double percent sign expands to directory of the current file
cnoremap %% <C-R>=expand('%:h').'/'<CR>

nmap <silent> <leader>h :wincmd h<CR>
nmap <silent> <leader>j :wincmd j<CR>
nmap <silent> <leader>k :wincmd k<CR>
nmap <silent> <leader>l :wincmd l<CR>

" o------------------o
" | Javascript stuff |
" o------------------o

autocmd FileType html set sw=2 ts=2 sts=2
autocmd FileType javascript set sw=2 ts=2 sts=2
autocmd FileType css set sw=2 ts=2 sts=2
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS

autocmd FileType javascript map <F3> :w<CR>:!nohup node % >> output.log &<CR>:!chromium-browser localhost:8080<CR><CR>
autocmd FileType javascript map <F4> :!killall -2 node<CR>

" o------------o
" | Java stuff |
" o------------o
autocmd Filetype java set makeprg=javac\ -cp\ .\ %
set shellpipe=>\ %s\ 2>&1
set errorformat=%A%f:%l:\ %m,%-Z%p^,%Csymbol\ \ :\ %m,%-C%.%#
autocmd FileType java map <F3> :w<CR>:make<CR><CR>:cw<CR>
autocmd FileType java map <F4> :!java -cp . %:r<CR><CR>
autocmd FileType java map <F5> :cprevious<CR>
autocmd FileType java map <F6> :cnext<CR>

" o--------------o
" | Haskell Mode |
" o--------------o

let g:ghc = "/usr/bin/ghc"

" For some reason this only works at the end
colorscheme solarized
