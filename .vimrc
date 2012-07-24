" Preamble / Vundle -------------------------------------------------------- {{{
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'altercation/vim-colors-solarized'
Bundle 'baskerville/bubblegum'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'mutewinter/vim-indent-guides'
Bundle 'Townk/vim-autoclose'
Bundle 'kien/ctrlp.vim'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/VimClojure'
Bundle 'tyok/js-mask'

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
set number
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
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
" Enable extended % matching
runtime macros/matchit.vim
" Prevents memory leaking from matches
autocmd BufWinLeave * call clearmatches()

set wildmenu

" Tabs, spaces, wrapping {{{
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set nowrap
set textwidth=80
set formatoptions+=qln
set formatoptions-=ro
" }}}

" Backups {{{
set undodir=~/.vim/tmp/undo//       " undo files
set backupdir=~/.vim/tmp/backup//   " backups
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
au FocusLost * :silent! wall            " Save when losing focus
au VimResized * :wincmd =               " Resize splits when window is resized

" Invisible characters {{{
set listchars=eol:¬,extends:>,precedes:<
" }}}

set synmaxcol=500   " Don't try to highlight lines longer than 500 characters

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

" Window movement commands
nnoremap <left>  :wincmd h<CR>
nnoremap <right> :wincmd l<CR>
nnoremap <up>    :wincmd k<CR>
nnoremap <down>  :wincmd j<CR>

"Double percent sign expands to directory of the current file
cnoremap %% <C-R>=expand('%:h').'/'<CR>

" Set C-j / C-k to scroll up and down half a screen
nnoremap <silent> <C-j> <C-d>
nnoremap <silent> <C-k> <C-u>

" Select (charwise) the contents of the current line, excluding indentation.
" Great for pasting Python lines into REPLs.
nnoremap vv ^vg_
" -------------------------------------------------------------------------- }}}

" Folding ------------------------------------------------------------------ {{{
set foldlevelstart=0

" Space to toggle folds
nnoremap <Space> za
vnoremap <Space> za

" Make zO recursively open the top level fold, period
nnoremap zO zCzO 

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
    return line . repeat(" ",fillcharcount) . '|' . foldedlinecount . '|' . ' '
endfunction " }}}
set foldtext=MyFoldText()
" -------------------------------------------------------------------------- }}}

" Plugin Settings ---------------------------------------------------------- {{{
" NERDTree {{{
nnoremap <F1> :NERDTreeToggle<CR>
inoremap <F1> <ESC>:NERDTreeToggle<CR>

augroup plugin_nerdtree
    au!
    au FileType nerdtree setlocal nolist
    au FileType nerdtree nnoremap <buffer> K :q<CR>
augroup END

let NERDTreeShowBookmarks=1     " Show bookmarks
let NERDTreeChDirMode=2         " Change the NERDTree directory to the root node
let NERDTreeMinimalUI=1
let NERDTreeDirArrows=1

let NERDTreeIgnore = ['.vim$', '\~$', 'tags.bak' ]
" }}}

" Syntastic {{{
let g:syntastic_javascript_jslint_conf = "--node --anon --sloppy --regex"
let g:syntastic_enable_signs=0
let g:syntastic_auto_jump=1
let g:syntastic_auto_loc_list=1
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
let g:ctrlp_map = '<C-p>'
let g:ctrlp_cmd = 'CtrlPMixed'
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

" Autoclose {{{
nmap <leader>x <Plug>ToggleAutoCloseMappings
" }}}

" Commentary {{{
nmap <leader>c <Plug>CommentaryLine
xmap <leader>c <Plug>Commentary

augroup plugin_commentary
    au!
    au FileType clojurescript setlocal commentstring=;\ %s
augroup END
" }}}

" Fugitive {{{
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gl :Git lg<CR>
nnoremap <leader>gw :Gwrite<CR>
nnoremap <leader>gc :Gcommit<CR>

" }}}
" -------------------------------------------------------------------------- }}}

" Filetype-specific -------------------------------------------------------- {{{
" Javascript {{{
augroup ft_javascript
    au!
    au FileType javascript setlocal foldmethod=marker
    au FileType javascript setlocal foldmarker={,}
    au FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    au FileType javascript nmap <F4> :w<CR>:make<CR>:cw<CR>
augroup END
" }}}

" HTML {{{
augroup ft_html
    au!
    au FileType html setlocal sw=2 ts=2 sts=2
augroup END
" }}}

" CSS {{{
augroup ft_css
    au!
    au BufNewFile,BufRead *.less setlocal filetype=less
    au Filetype less,css setlocal foldmethod=marker
    au FileType less,css setlocal foldmarker={,}
    au FileType less,css setlocal omnifunc=csscomplete#CompleteCSS
    au FileType less,css setlocal iskeyword+=-

    " Map <leader>S to sort CSS properties. Badass.
    au BufNewFile,BufRead *.less,*.css nnoremap <buffer> <localleader>S ?{<CR>jV/\v^\s*\}?$<CR>k:sort<CR>:noh<CR>
augroup END
" }}}

" Clojure {{{
augroup ft_clojure
    " Make AutoClose stop adding extra apostrophes
    au Filetype clojure let b:AutoClosePairs = AutoClose#DefaultPairsModified("", "'")
" }}}
"
" Java {{{
augroup ft_java
    au Filetype java setlocal makeprg=javac\ -cp\ .\ %
    au Filetype java setlocal shellpipe=>\ %s\ 2>&1
    au Filetype java setlocal errorformat=%A%f:%l:\ %m,%-Z%p^,%Csymbol\ \ :\ %m,%-C%.%#
    au FileType java nnoremap <buffer> <F2> :w<CR>:make<CR><CR>:cw<CR>
    au FileType java nnoremap <buffer> <F3> :!java -cp . %:r<CR><CR>
    au FileType java nnoremap <buffer> <F4> :cprevious<CR>
    au FileType java nnoremap <buffer> <F5> :cnext<CR>
augroup END
" }}}

" Markdown {{{
augroup ft_markdown
    au!
    " Use <localleader>1/2/3 to add headings.
    au Filetype markdown nnoremap <buffer> <localleader>1 I### <ESC>
    au Filetype markdown nnoremap <buffer> <localleader>2 I#### <ESC>
    au Filetype markdown nnoremap <buffer> <localleader>3 I##### <ESC>
augroup END
" }}}

" QuickFix {{{

augroup ft_quickfix
    au!
    au Filetype qf setlocal colorcolumn=0 nolist nocursorline nowrap tw=0
augroup END

" }}} Vim {{{
augroup ft_vim
    au!

    au FileType vim setlocal foldmethod=marker
    au FileType help setlocal textwidth=78
    au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
augroup END
" -------------------------------------------------------------------------- }}}

" -------------------------------------------------------------------------- }}}

" GUI / OS Specific -------------------------------------------------------- {{{
if has("gui_running")
    set guioptions=
    set guifont=Dina\ 10
    set cursorline
    set list
endif

" Bells, go the hell away please
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

" -------------------------------------------------------------------------- }}}

if $TERM == "screen-256color"
    colorscheme bubblegum
else
    set background=dark
    colorscheme solarized
endif
