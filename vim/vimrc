"====================================================
"               VIM DEFAULT CONFIGS
"====================================================

set nocompatible
set number
set nowrap
set nobackup
set shiftwidth=4
set tabstop=4
syntax on
filetype plugin on
filetype plugin indent on
let mapleader = "," 
set background=dark


"====================================================
"                  VUNDLE SETUP
"====================================================

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
	Plugin 'VundleVim/Vundle.vim'

	Plugin 'eagletmt/ghcmod-vim'
	Plugin 'Shougo/vimproc.vim'
	Plugin 'derekwyatt/vim-scala'

	"Vim Functionality
	Plugin 'ensime/ensime-vim'
	Plugin 'junegunn/fzf'

	"Web Dev
	Plugin 'othree/html5.vim'
	Plugin 'pangloss/vim-javascript'
	Plugin 'JulesWang/css.vim'
	Plugin 'cakebaker/scss-syntax.vim'
	Plugin 'godlygeek/tabular'
	Plugin 'plasticboy/vim-markdown'
	Plugin 'elzr/vim-json'
	Plugin 'stephpy/vim-yaml'
call vundle#end()


"====================================================
"                 PLUGIN CONFIGS
"====================================================


"-----------HASKELL-------------
noremap <Leader>ht :GhcModType<cr>
nnoremap <Leader>htc :GhcModTypeClear<cr>


"------------SCALA-----------------
autocmd BufWritePost *.scala silent :EnTypeCheck
au FileType scala nnoremap <localleader>df :EnDeclaration<CR>
