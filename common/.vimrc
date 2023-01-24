"=============================================================================
" .vimrc
"
" Vim Config
"
" Author: Kyle W T Sherman <kylewsherman@gmail.com>
"=============================================================================

" UI
colorscheme industry                    " color scheme
set cursorline                          " highlight line with cursor
syntax on                               " syntax highlighting
set number                              " show line numbers
"set relativenumber                      " show relative line numbers
set showcmd                             " show command in bottom bar
set wildmenu                            " visual autocomplete for command menu
set lazyredraw                          " only redraw when needed
set showmatch                           " highlight parenthesis matching [{()}]

" Tabs
set tabstop=4                           " number of spaces per TAB
set softtabstop=4                       " number of spaces in tab when editing
set autoindent                          " automatically indent lines based on syntax
filetype indent on                      " load filetype-specific indent files
set expandtab                           " replace tabs with spaces
set shiftwidth=4                        " number of spaces when shifting blocks

" Search
set incsearch                           " search as characters are entered
set hlsearch                            " highlight search matches

" Keybindings
" turn off search highlight
nnoremap <leader><space> :nohlsearch<CR>

"=============================================================================
" End of File
"=============================================================================
