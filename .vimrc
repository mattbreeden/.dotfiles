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

set incsearch

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
set tabstop=4
set shiftwidth=4

" case insensitive / changes to
" case sensitive with capital letter
set ignorecase
set smartcase

set colorcolumn=80

" explicitly set filetype to Ruby
au BufRead,BufNewFile {Gemfile,Rakefile,Vagrantfile,Thorfile,Procfile,Capfile,Guardfile,config.ru,.railsrc,.irbrc,.pryrc} set ft=ruby

" 4 spaces for TAB in Python files
autocmd BufEnter *.rb,*.erb setlocal softtabstop=2 shiftwidth=2

set t_Co=256
let base16colorspace=256  " Access colors present in 256 colorspace
colorscheme base16-tomorrow
set background=dark
" colorscheme molokai

au InsertEnter * set cursorline
au InsertLeave * set nocursorline
" This is for if the cursorline commands above are switched
" augroup CursorLine
    " au!
    " au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    " au WinLeave * setlocal nocursorline
" augroup END

set laststatus=2
let mapleader = ","

noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" resize current buffer by +/- 5 
nnoremap - :resize -5<cr>
nnoremap + :resize +5<cr>
" Think of mapping that doesn't conflict
" nnoremap > :vertical resize -5<cr>
" nnoremap < :vertical resize +5<cr>

" have Y behave analogously to D and C rather than to dd and cc which is
" already done by yy
noremap Y y$

inoremap jk <Esc>

" 'unbind' K which opens manpages for word under cursor
map <S-k> <Nop>

nmap <c-s> :wa<CR>
vmap <c-s> <Esc><c-s>gv
imap <c-s> <Esc><c-s>

" Have search traversal always center the screen
nmap n nzz
nmap N Nzz

set wildignore+=*.pyc,*.dat
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_custom_ignore = {
  \ 'dir': '\v[\/](\.git|node_modules|bower_components)$',
  \ }
nnoremap <leader>p :CtrlPTag<cr>
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

" Old rails test binds
" let g:no_turbux_mappings = 1
" map <leader>rt <Plug>SendTestToTmux
" map <leader>rT <Plug>SendFocusedTestToTmux
" map <Leader>t :call RunCurrentSpecFile()<CR>
" map <Leader>s :call RunNearestSpec()<CR>
" map <Leader>l :call RunLastSpec()<CR>

abbreviate ipry require IEx; IEx.pry
abbreviate rpry require 'pry'; binding.pry
abbreviate ipy import IPython; IPython.embed()
abbreviate pdb import pdb; pdb.set_trace()

function! PromoteToLet()
  :.s/\(\w\+\) = \(.*\)$/let(:\1) { \2 }/
  :normal ==
endfunction
:command! PromoteToLet :call PromoteToLet()
:map <Leader><C-p> :PromoteToLet<cr>

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
      \ "fabrication": {
      \   "spec/fabricators/*_fabricator.rb": {
      \     "command": "fabricator",
      \     "related": "app/models/%s.rb",
      \     "template": "Fabricator(:%S) do\nend"
      \   }
      \ },
      \ "factory_girl_rails": {
      \   "spec/factories.rb": {
      \     "command": "factories",
      \     "template": "FactoryGirl.define do\nend"
      \   }
      \ }}

let g:slimv_swank_cmd = '! xterm -e sbcl --load ~/.vim/bundle/slimv/slime/start-swank.lisp &'
let g:lisp_rainbow=1

"C Completion
let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'

" Have nerdcommenter insert a space after the comment symbols
let g:NERDSpaceDelims=1

let g:UltiSnipsExpandTrigger="<c-e>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:ycm_path_to_python_interpreter = '/usr/bin/python'
let g:ycm_key_list_select_completion=['<TAB>', '<DOWN>']
let g:ycm_key_list_previous_completion=['<S-TAB>', '<Up>']
" TODO: ugh
"let g:ycm_add_preview_to_completeopt=1
"let g:ycm_autoclose_preview_window_after_insertion=1

let g:EasyMotion_smartcase = 1
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
" nmap f <Plug>(easymotion-s)
