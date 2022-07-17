set path +=**

execute "set t_8f=\e[38;2;%lu;%lu;%lum"
execute "set t_8b=\e[48;2;%lu;%lu;%lum"

set exrc
set guicursor=
set nu
set rnu
set nobomb
set termguicolors
set nohlsearch
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
let mapleader = " "
set noshowmode
set incsearch
set hidden
set smartcase
set undodir=~/.config/nvim/undodir
set undofile
set scrolloff=8
set showmatch
set encoding=utf-8
set nowrap
set noerrorbells
set completeopt=menuone,noinsert,noselect
set noswapfile
set nobackup
set clipboard=unnamedplus

set signcolumn=yes
set colorcolumn=80

set cmdheight=2

set updatetime=50
set shortmess+=c

syntax enable

inoremap " ""<left>
inoremap ' ''<left>
inoremap ( ()<left>
inoremap { {}<left>
inoremap [ []<left>

call plug#begin()
Plug 'nvim-telescope/telescope-file-browser.nvim'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'morhetz/gruvbox'
Plug 'CreaturePhil/vim-handmade-hero'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'ThePrimeagen/harpoon'
Plug 'nvim-telescope/telescope-fzf-native.nvim', {'do': 'make'}
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
call plug#end()

colorscheme handmade-hero
highligh Normal guibg=None

nnoremap <leader>tt :Telescope keymaps<CR>

nnoremap <leader>/ :Telescope current_buffer_fuzzy_find<CR>

" Harpoon remaps
nnoremap <leader>haf :lua require("harpoon.mark").add_file()<CR>
nnoremap <leader>hm :Telescope harpoon marks<CR>
nnoremap <leader>h1 :lua require("harpoon.ui").nav_file(1)<CR>
nnoremap <leader>h2 :lua require("harpoon.ui").nav_file(2)<CR>
nnoremap <leader>h3 :lua require("harpoon.ui").nav_file(3)<CR>
nnoremap <leader>h4 :lua require("harpoon.ui").nav_file(4)<CR>
nnoremap <leader>hp :lua require("harpoon.ui").nav_prev()<CR>
nnoremap <leader>hn :lua require("harpoon.ui").nav_next()<CR>

" Harpoon remove files
nnoremap <leader>hd1 :lua require("harpoon.mark").rm_file(1)<CR>
nnoremap <leader>hd2 :lua require("harpoon.mark").rm_file(2)<CR>
nnoremap <leader>hd3 :lua require("harpoon.mark").rm_file(3)<CR>
nnoremap <leader>hd4 :lua require("harpoon.mark").rm_file(4)<CR>

lua require('zensokushi')
