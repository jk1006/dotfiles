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
Plug 'arcticicestudio/nord-vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()

colorscheme nord
filetype plugin indent on    " require

" let NERDTree show hidden files and directories and line numbers
let NERDTreeShowHidden=1
let NERDTreeShowLineNumbers=1

" shortcut to toggle nerdtree
map <C-n> :NERDTreeToggle<CR>

let g:airline_theme = 'minimalist'

let g:vimwiki_list = [{'path': '/Users/d068796/pCloud Drive/vimwiki'}]
tnoremap <Esc> <C-\><C-n>
" map Leader y and p to copy / paste from clipboard
nnoremap <Leader>p "*P
nnoremap <Leader>y "*y
nnoremap <Leader>Y "*Y

nnoremap <Leader>o :Prettier<CR>

nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR>


map <C-p> :GFiles<CR>
" hotkey for splitting windows
map <Leader>s :sp<CR>
map <Leader>v :vs<CR>
map <Leader>c :q<CR>
map <Leader>t :50sp<CR><C-j>:terminal<CR>i

map <Leader>< :vertical resize +5<CR>
map <Leader>> :vertical resize -5<CR>
map <Leader>+ :resize +5<CR>
map <Leader>- :resize -5<CR>

" map keys for easier window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

" Using lua functions
nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<cr>
nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>

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
set shell=/usr/local/bin/zsh

map <leader>i :call mdip#MarkdownClipboardImage()<CR>
let g:mdip_imgdir = '.'

let g:vimwiki_list = [{'path': '~/pCloudDrive/vimwiki',
                      \ 'syntax': 'markdown', 'ext': '.md'}]

" https://github.com/neoclide/coc.nvim#example-vim-configuration
inoremap <silent><expr> <c-space> coc#refresh()

" gd - go to definition of word under cursor
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)

" gi - go to implementation
nmap <silent> gi <Plug>(coc-implementation)

" gr - find references
nmap <silent> gr <Plug>(coc-references)

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
