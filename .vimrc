set nocompatible

set encoding=utf-8
set fileformats=unix
set ff=unix
set nomodeline

set undodir^=~/.vimtemp/undo
set backupdir^=~/.vimtemp/backup
set directory^=~/.vimtemp/temp
set nobackup
set noswapfile

set wildmenu

set autoread
set autowrite

set ttyfast
set mouse=a

execute pathogen#infect()
call pathogen#incubate()
syntax on
filetype plugin indent on

set scrolloff=3
set wrap
set nu
set expandtab
set tabstop=2
set shiftwidth=2

" explicitly set filetype to Ruby
au BufRead,BufNewFile {Gemfile,Rakefile,Vagrantfile,Thorfile,Procfile,Capfile,Guardfile,config.ru,.railsrc,.irbrc,.pryrc} set ft=ruby

" 4 spaces for TAB in Python files
autocmd BufEnter *.py setlocal softtabstop=4 shiftwidth=4

set t_Co=256
colorscheme molokai

au InsertEnter * set cursorline
au InsertLeave * set nocursorline

set laststatus=2

let mapleader = ","

noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" have Y behave analogously to D and C rather than to dd and cc (which is
" already done by yy)
noremap Y y$

nmap <c-s> :wa<CR>
vmap <c-s> <Esc><c-s>gv
imap <c-s> <Esc><c-s>

let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlPMixed'
nmap <silent> <leader>b :TagbarToggle<CR>

" Vimux
let g:VimuxUseNearestPane = 1
" Prompt for a command
map <leader>rp :VimuxPromptCommand<CR>
" Run last command
map <leader>rl :VimuxRunLastCommand<CR>
" Inspect runner pane
map <leader>ri :VimuxInspectRunner<CR>
" Close Vimux Runner
map <leader>rc :VimuxCloseRunner<CR>
" Interrupt any command in the runner pane
map <leader>rs :VimuxInterruptRunner<CR>

let g:no_turbux_mappings = 1
map <leader>rt <Plug>SendTestToTmux
map <leader>rT <Plug>SendFocusedTestToTmux

map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>l :call RunLastSpec()<CR>

abbreviate rpry require 'pry'; binding.pry

function! PromoteToLet()
  :.s/\(\w\+\) = \(.*\)$/let(:\1) { \2 }/
  :normal ==
endfunction
:command! PromoteToLet :call PromoteToLet()
:map <Leader>p :PromoteToLet<cr>

" Mustache/Handlebars shortcuts
let g:mustache_abbreviations = 1

" #######################################
" Rails Projections
" #######################################
let g:rails_gem_projections = {
      \ "active_model_serializers": {
      \   "app/serializers/*_serializer.rb": {
      \     "command": "serializer",
      \     "affinity": "model",
      \     "test": "spec/serializers/%s_spec.rb",
      \     "related": "app/models/%s.rb",
      \     "template": "class %SSerializer < ActiveModel::Serializer\nend"
      \   }
      \ },
      \ "draper": {
      \   "app/decorators/*_decorator.rb": {
      \     "command": "decorator",
      \     "affinity": "model",
      \     "test": "spec/decorators/%s_spec.rb",
      \     "related": "app/models/%s.rb",
      \     "template": "class %SDecorator < Draper::Decorator\nend"
      \   }
      \ },
      \ "factory_girl_rails": {
      \   "spec/factories.rb": {
      \     "command": "factories",
      \     "template": "FactoryGirl.define do\nend"
      \   }
      \ }}
