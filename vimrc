" Remap k,j to move within wrapped lines.
noremap <silent> k gk
noremap <silent> j gj
noremap <silent> 0 g0
noremap <silent> $ g$

" Better search.
set hlsearch
set ignorecase

" Mute vim.
set belloff=all

" Better tabs.
set tabstop=4 softtabstop=0 expandtab shiftwidth=4 smarttab

" Enable line numbers
set number

" Experimentals.
syntax on
filetype plugin on
set history=50
set ruler
set backup
set nocompatible
set bs=indent,eol,start
