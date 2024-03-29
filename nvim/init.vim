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
Plug 'tpope/vim-surround'
Plug 'chrisbra/improvedft'
Plug 'mhinz/vim-startify'
Plug 'rust-lang/rust.vim'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzy-native.nvim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-compe'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'kyazdani42/nvim-web-devicons' " for file icons
Plug 'kyazdani42/nvim-tree.lua'
call plug#end()

filetype plugin indent on    " require
" use a slightly darker background, like GitHub inline code blocks

set t_Co=256   " This is may or may not needed.
let g:github_colors_soft = 1
set background=dark
colorscheme palenight
" let NERDTree show hidden files and directories and line numbers
let NERDTreeShowHidden=1
let NERDTreeShowLineNumbers=1

" shortcut to toggle nerdtree
map <C-n> :NERDTreeToggle<CR>

let g:airline_theme = 'ayu_dark'

tnoremap <Esc> <C-\><C-n>
" map Leader y and p to copy / paste from clipboard
nnoremap <Leader>p "*P
nnoremap <Leader>y "*y
nnoremap <Leader>Y "*Y

nnoremap <C-n> :NvimTreeToggle<CR>
nnoremap <leader>r :NvimTreeRefresh<CR>


nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR>


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
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>gf <cmd>Telescope git_files<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>
nnoremap <leader>gr <cmd>Telescope lsp_references<cr>
nnoremap <leader>ca <cmd>Telescope lsp_code_actions<cr>
nnoremap <leader>wd <cmd>Telescope lsp_workspace_diagnostics<cr>
nnoremap <leader>gd <cmd>Telescope lsp_definitions<cr>
nnoremap <leader>gi <cmd>Telescope lsp_implementations<cr>

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

let g:mdip_imgdir = '.'

nnoremap <expr> k (v:count > 1 ? "m'" . v:count : '') . 'k'
nnoremap <expr> j (v:count > 1 ? "m'" . v:count : '') . 'j'
"cycle through buffers
nnoremap <leader>bn :bn<cr>
nnoremap <leader>bp :bp<cr>
nnoremap <leader>bk :bd<cr>

inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-e>     compe#close('<C-e>')
inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })



nmap <silent> K :lua vim.lsp.buf.hover()<CR>
nmap <silent> gd :lua vim.lsp.buf.definition()<CR>
nmap <silent> gi :lua vim.lsp.buf.implementation()<CR>
nmap <silent> gr :lua vim.lsp.buf.references()<CR>
nnoremap <silent>ca <cmd>lua vim.lsp.buf.code_action()<CR>
nnoremap <leader>rn :lua vim.lsp.buf.rename()<CR>
map <C-j> <cmd>lua vim.lsp.diagnostic.goto_next()<CR>
map <C-k> <cmd>lua vim.lsp.diagnostic.goto_prev()<CR>
nnoremap <leader>f <cmd>lua vim.lsp.buf.format()<CR>

autocmd BufNewFile,BufRead *.org setf dotoo " make dotoo work with .org

lua << EOF
local nvim_lsp = require('lspconfig')
require'lspconfig'.rust_analyzer.setup{}
require'lspconfig'.tsserver.setup{}
require'lspconfig'.pyright.setup{}
vim.o.completeopt = "menuone,noselect"
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Mappings.
  local opts = { noremap=true, silent=true }
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
end

require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = true;

  source = {
    path = true;
    buffer = true;
    calc = true;
    nvim_lsp = true;
    nvim_lua = true;
  };
}
EOF
