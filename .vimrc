" Preamble / Vundle -------------------------------------------------------- {{{
filetype off
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

set t_Co=256
set nocompatible
filetype plugin indent on
syntax enable
" -------------------------------------------------------------------------- }}}
" Basic Options ------------------------------------------------------------ {{{
set encoding=utf-8
set modelines=0
set autoindent
set showmode
set showcmd
set hidden
set ttyfast
set ruler
set backspace=indent,eol,start
set nonumber
set norelativenumber
set laststatus=2
set history=1000
set undofile
set undoreload=10000
set shell=/bin/zsh
set lazyredraw
set matchtime=3
set splitbelow
set splitright
set autowrite
set autoread
set autochdir
set shiftround
set title
set linebreak

" Enable extended % matching
runtime macros/matchit.vim

" Tabs, spaces, wrapping {{{
set tabstop=8
set shiftwidth=4
set softtabstop=4
set expandtab
set wrap
set textwidth=80
set formatoptions=qrn1
" }}}

" Backups {{{
set undodir=~/.vim/tmp/undo//       " undo files
set backupdir=~/.vim/tmp/backup//   " backups
set directory=~/.vim/tmp/swap//     " swap files
set backup                          " enable backups
set noswapfile                      " no swap files
" }}}

" Searching and regexes {{{
set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
set gdefault

set scrolloff=3
set sidescroll=1
set sidescrolloff=10
" }}}

set completeopt=longest,menuone,preview " Better completion
au FocusLost * :silent! wall    " Save when losing focus
au VimResized * :wincmd =       " Resize splits when window is resized

" Invisible characters {{{
set list
set listchars=eol:¬,extends:>,precedes:<
augroup trailing
    au!
    au InsertEnter * :set listchars-=trail:¿
    au InsertLeave * :set listchars-=trail:¿
augroup END
" }}}

set synmaxcol=500 " Don't try to highlight lines longer than 500 characters

"Time out on key codes but not mappings (for terminal Vim)
set notimeout
set ttimeout
set ttimeoutlen=10
" -------------------------------------------------------------------------- }}}
" Key Mappings ------------------------------------------------------------- {{{
let maplocalleader = ","

" Insert mode completion
inoremap <C-f> <C-x><C-f>
inoremap <C-]> <C-x><C-]>

nnoremap <left>  :cprev<cr>zvzz
nnoremap <right> :cnext<cr>zvzz
nnoremap <up>    :lprev<cr>zvzz
nnoremap <down>  :lnext<cr>zvzz

"Double percent sign expands to directory of the current file
cnoremap %% <C-R>=expand('%:h').'/'<CR>

" Set C-j / C-k to scroll up and down half a screen
nnoremap <silent> <C-j> <C-d>
nnoremap <silent> <C-k> <C-u>

" Buffer movement commands
nnoremap <silent> <leader>h :wincmd h<CR>
nnoremap <silent> <leader>j :wincmd j<CR>
nnoremap <silent> <leader>k :wincmd k<CR>
nnoremap <silent> <leader>l :wincmd l<CR>
nnoremap <leader>v <C-w>v

" Select (charwise) the contents of the current line, excluding indentation.
" Great for pasting Python lines into REPLs.
nnoremap vv ^vg_
" -------------------------------------------------------------------------- }}}
" Folding ------------------------------------------------------------------ {{{
set foldlevelstart=0

" Space to toggle folds
nnoremap <Space> za
vnoremap <Space> za

nnoremap zO zCzO " Make zO recursively open the top level fold, period

function! MyFoldText() " {{{
    let line = getline(v:foldstart)

    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart

    " expand tabs into spaces
    let onetab = strpart('          ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')

    let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
    let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
    return line . '¿' . repeat(" ",fillcharcount) . foldedlinecount . '¿' . ' '
endfunction " }}}
set foldtext=MyFoldText()
" -------------------------------------------------------------------------- }}}
" Plugin Settings ---------------------------------------------------------- {{{
" NERDTree {{{
nnoremap <F1> :NERDTreeToggle<CR>
let NERDTreeShowBookmarks=1
let NERDTreeChDirMode=2 " Change the NERDTree directory to the root node
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
" }}}

" Indent Guides {{{
if has("gui_running")
    let g:indent_guides_auto_colors=1
    let g:indent_guides_enable_on_vim_startup=1
    let g:indent_guides_guide_size=4
    let g:indent_guides_color_change_percent=5
endif
" }}}
" Ctrl-P {{{

let g:ctrlp_dont_split = 'NERD_tree_2'
let g:ctrlp_jump_to_buffer = 0
let g:ctrlp_map = '<leader>,'
let g:ctrlp_working_path_mode = 0
let g:ctrlp_match_window_reversed = 1
let g:ctrlp_split_window = 0
let g:ctrlp_max_height = 20
let g:ctrlp_extensions = ['tag']

let g:ctrlp_prompt_mappings = {
\ 'PrtSelectMove("j")':   ['<c-j>', '<down>', '<s-tab>'],
\ 'PrtSelectMove("k")':   ['<c-k>', '<up>', '<tab>'],
\ 'PrtHistory(-1)':       ['<c-n>'],
\ 'PrtHistory(1)':        ['<c-p>'],
\ 'ToggleFocus()':        ['<c-tab>'],
\ }

let ctrlp_filter_greps = "".
    \ "egrep -iv '\\.(" .
    \ "jar|class|swp|swo|log|so|o|pyc|jpe?g|png|gif|mo|po" .
    \ ")$' | " .
    \ "egrep -v '^(\\./)?(" .
    \ "deploy/|lib/|classes/|libs/|deploy/vendor/|.git/|.hg/|.svn/|.*migrations/" .
    \ ")'"

let my_ctrlp_user_command = "" .
    \ "find %s '(' -type f -or -type l ')' -maxdepth 15 -not -path '*/\\.*/*' | " .
    \ ctrlp_filter_greps

let my_ctrlp_git_command = "" .
    \ "cd %s && git ls-files | " .
    \ ctrlp_filter_greps

let g:ctrlp_user_command = ['.git/', my_ctrlp_git_command, my_ctrlp_user_command]

nnoremap <leader>. :CtrlPTag<cr>

" }}}
" -------------------------------------------------------------------------- }}}
" Filetype-specific -------------------------------------------------------- {{{
" Javascript {{{
autocmd FileType html set sw=2 ts=2 sts=2
autocmd FileType javascript set sw=2 ts=2 sts=2
autocmd FileType css set sw=2 ts=2 sts=2
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS

autocmd FileType javascript map <F3> :w<CR>:!nohup node % >> output.log &<CR>:!chromium-browser localhost:8080<CR><CR>
autocmd FileType javascript map <F4> :!killall -2 node<CR>
" }}}

" Java {{{
autocmd Filetype java set makeprg=javac\ -cp\ .\ %
set shellpipe=>\ %s\ 2>&1
set errorformat=%A%f:%l:\ %m,%-Z%p^,%Csymbol\ \ :\ %m,%-C%.%#
autocmd FileType java map <F3> :w<CR>:make<CR><CR>:cw<CR>
autocmd FileType java map <F4> :!java -cp . %:r<CR><CR>
autocmd FileType java map <F5> :cprevious<CR>
autocmd FileType java map <F6> :cnext<CR>
" }}}

" Color scheme {{{
if has("gui_running")
    set guioptions=
    set guifont=Dina\ 10
    set cursorline
else
    set t_Co=256
endif

if has('win32') || has('win64')
    set lines=50
    set columns=80
    winpos 0 0 
    set guifont=Consolas:h10:cANSI
    source $VIMRUNTIME/mswin.vim
    behave mswin
    cd ~
endif

syntax on
set background=dark
colorscheme solarized
" }}}
