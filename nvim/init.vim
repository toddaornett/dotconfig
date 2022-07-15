"========================================
" plugin automatic first installation
"========================================

let plug_install = 0
let autoload_plug_path = stdpath('config') . '/autoload/plug.vim'
if !filereadable(autoload_plug_path)
    silent exe '!curl -fL --create-dirs -o ' . autoload_plug_path . 
        \ ' https://raw.github.com/junegunn/vim-plug/master/plug.vim'
    execute 'source ' . fnameescape(autoload_plug_path)
    let plug_install = 1
endif
unlet autoload_plug_path

call plug#begin('~/.config/nvim/plugins')
Plug 'tpope/vim-fugitive'
Plug 'tyru/open-browser.vim' " opens url in browser
Plug 'http://github.com/tpope/vim-surround' " Surrounding ysw)
Plug 'https://github.com/preservim/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'https://github.com/ap/vim-css-color' " CSS Color Preview
Plug 'https://github.com/tpope/vim-commentary' " For Commenting gcc & gc
Plug 'SirVer/ultisnips'
Plug 'thirtythreeforty/lessspace.vim'
Plug 'vim-airline/vim-airline'
Plug 'preservim/nerdtree'
call plug#end()

if plug_install
    PlugInstall --sync
endif
unlet plug_install


"========================================
" general
"========================================

filetype on
filetype plugin on
filetype plugin indent on
syntax on
set smartindent
set expandtab               "tab to spaces
set tabstop=4               "the width of a tab
set shiftwidth=4            "the width for indent
set foldenable
set foldmethod=indent       "folding by indent
set foldlevel=99
set ignorecase              "ignore case for search
set smartcase               "for search with any uppercase text
set noswapfile
set nobackup                " disable backup
set nu                      " line number display
set wildmode=longest,list   " get bash-like tab completions
set colorcolumn=80          " set an 80 column border for good coding style
set cursorline              " highlight current cursorline
set incsearch               " incremental search
set scrolloff=8             " threshold in lines for scrolling
set signcolumn=yes          " show column of meta, say error, info
set cmdheight=2             " give more space for displaying messages
set mouse=a

augroup ReloadAndReformatVimrc
    autocmd!
    autocmd BufWritePre *.vim,~/.vimrc,vimrc :normal m""gg=G<C-o><C-o>`""
    autocmd BufWritePost *.vim,~/.vimrc,vimrc :source $MYVIMRC
augroup END

let mapleader = ' '
hi LineNr ctermfg=darkblue ctermbg=grey
hi CursorLineNr ctermfg=45 cterm=bold

"========================================
" plugin configuration
"========================================

" config snippets plugin
let g:UltiSnipsSnippetStorageDirectoryForUltiSnipsEdit=$HOME.'/.config/local-nvim-snippets'
let g:UltiSnipsSnippetDirectories=["UltiSnips", $HOME.'/.config/local-nvim-snippets']
let g:UltiSnipsExpandTrigger="<tab>"
" list all snippets for current filetype
let g:UltiSnipsListSnippets="<c-l>"


" config lessspace plugin
let g:lessspace_blacklist = ['markdown']


" config nerdtree plugin
map <silent> <C-n> :NERDTreeFocus<CR>
map <C-o> :NERDTreeToggle %<CR>


"========================================
" custom commands
"========================================

" edit config file
command! Edrc edit $HOME/.config/nvim/init.vim"


"========================================
" keymap
"========================================

" terminal for intuitively switching windows
tnoremap <C-w>h <C-\><C-n><C-w>h
tnoremap <C-w>j <C-\><C-n><C-w>j
tnoremap <C-w>k <C-\><C-n><C-w>k
tnoremap <C-w>l <C-\><C-n><C-w>l

" window keymap
" Source: http://stackoverflow.com/a/6404246/151007
let i = 1
" If I have more than 9 windows open I have bigger problems :)
while i <= 9
    execute 'nnoremap <Leader>'.i.' :'.i.'wincmd w<CR>'
    let i = i + 1
endwhile
