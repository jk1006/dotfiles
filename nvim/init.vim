set runtimepath^=~/.vim runtimepath+=~/.vim/after         
    let &packpath = &runtimepath                                                
    source ~/.vimrc  
set backspace=2  " Backsapce deletes character"
" always turn on syntax check
syntax on 
" map sapce as leader key
let mapleader=" "
set nobackup
set nowritebackup
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set undodir=~/.vim/undodir
" no line wrapping, scrolling
set nowrap
" buffers can be closed without saving
set hidden
"keep 8 lines while scrolling
set scrolloff=8
set undofile
set incsearch
set noswapfile
set history=50
set autoread " always read changes
set autowrite
set ruler
set number relativenumber
set rnu
set nocompatible              " be iMproved, required
set clipboard=unnamed " add macos clipboard support
set splitbelow
set termguicolors
filetype off                  " required

" remove bars between vertical splits
highlight VertSplit cterm=NONE

  " When editing a file, always jump to the last known cursor position.
  " Don't do it for commit messages, when the position is invalid, or when
  " inside an event handler (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  " Set syntax highlighting for specific file types
  autocmd BufRead,BufNewFile *.md set filetype=markdown
  autocmd BufRead,BufNewFile .{jscs,jshint,eslint}rc set filetype=json
  autocmd BufRead,BufNewFile aliases.local,zshrc.local,*/zsh/configs/* set filetype=sh
  autocmd BufRead,BufNewFile gitconfig.local set filetype=gitconfig
  autocmd BufRead,BufNewFile tmux.conf.local set filetype=tmux
  autocmd BufRead,BufNewFile vimrc.local set filetype=vim

call plug#begin()

Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-surround'
Plug 'chrisbra/improvedft'
Plug 'mhinz/vim-startify'
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
Plug 'rust-lang/rust.vim'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzy-native.nvim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'rakr/vim-one'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'tpope/vim-fugitive'
call plug#end()

filetype plugin indent on    " require

set t_Co=256   " This is may or may not needed.
let g:github_colors_soft = 1
set background=dark
colorscheme one

"
let NERDTreeShowHidden=1
let NERDTreeShowLineNumbers=1

" shortcut to toggle nerdtree
map <C-n> :NERDTreeToggle<CR>

let g:airline_theme = 'one'

tnoremap <Esc> <C-\><C-n>
" map Leader y and p to copy / paste from clipboard
nnoremap <Leader>p "*P
nnoremap <Leader>y "*y
nnoremap <Leader>Y "*Y

" hotkey for splitting windows
map <Leader>s :sp<CR>
map <Leader>v :vs<CR>
map <Leader>c :q<CR>
map <Leader>t :10sp<CR><C-w>j:terminal<CR>i

map <Leader>< :vertical resize +5<CR>
map <Leader>> :vertical resize -5<CR>
map <Leader>+ :resize +5<CR>
map <Leader>- :resize -5<CR>


" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>gf <cmd>Telescope git_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction
" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" map Y to copy rest of line
map Y y$

filetype plugin on
set shell=/usr/bin/zsh

" https://github.com/neoclide/coc.nvim#example-vim-configuration
inoremap <silent><expr> <c-space> coc#refresh()


" gh - get hint on whatever's under the cursor
nnoremap <silent> K :call <SID>show_documentation()<CR>
nnoremap <silent> gh :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" cycle through coc errors and warnings
nmap <silent> <C-k> <Plug>(coc-diagnostic-prev)
nmap <silent> <C-j> <Plug>(coc-diagnostic-next)
nnoremap <leader>q :CocDiagnostics<cr>

xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

"cycle through buffers
nnoremap <leader>bn :bn<cr>
nnoremap <leader>bp :bp<cr>
nnoremap <leader>bk :bd<cr>
"" enable scrolling in CoC popups
nnoremap <nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
inoremap <nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
