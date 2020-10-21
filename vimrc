call plug#begin("/home/dinatamas/.vim/plugged")

Plug 'altercation/vim-colors-solarized'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

" Vim Airline (lightweight Powerline).
"" Always show the status line.
set laststatus=2
"" Increase command line height.
set cmdheight=2
"" Set theme.
let g:airline_theme='solarized'
let g:airline_solarized_bg='dark'
" Use correct fonts.
let g:airline_powerline_fonts = 1

" Use 16 colors.
set t_Co=16
" Use solarized theme.
set background=dark
colorscheme solarized

" Remap k,j to move within wrapped lines.
noremap <silent> k gk
noremap <silent> j gj
noremap <silent> 0 g0
noremap <silent> $ g$

" Default to UTF-8.
set encoding=utf-8

" Better search.
set incsearch
set hlsearch
set ignorecase
"" Use <C-L> to clear the search highlights.
nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>

" Mute bell.
set belloff=all

" Better tabs.
set autoindent
set copyindent
set tabstop=4 softtabstop=0 expandtab shiftwidth=4 smarttab

" Enable line numbers.
set number

" vim and tmux interoperability.
set background=dark

" Experimentals.
syntax on
filetype plugin on
filetype indent on
set history=50
set ruler
set wildmenu
set backup
set nocompatible
set bs=indent
" set whichwrap=b,s,<,>,[,] " To allow the left/right arrows keys to move between lines
set complete-=i
set nrformats-=octal
set scrolloff=5 " Always show 5 lines after cursor.
set sidescrolloff=5 " Always show 5 columns next to cursor.
set display+=lastline
