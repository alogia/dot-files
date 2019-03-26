set nocompatible              " be iMproved, required
filetype off                  " required
set number
set nowrap
set nobackup
set shiftwidth=4
set tabstop=4
syntax on
filetype plugin on
filetype plugin indent on

set background=dark

" set the runtime path to include Vundle and initialize
"-----------------------------------------------------
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
	" let Vundle manage Vundle, required
	Plugin 'VundleVim/Vundle.vim'

	Plugin 'eagletmt/ghcmod-vim'
	Plugin 'Shougo/vimproc.vim'
	Plugin 'derekwyatt/vim-scala'

	"Vim Functionality
	

	"Web Dev
	Plugin 'othree/html5.vim'
	Plugin 'pangloss/vim-javascript'
	Plugin 'JulesWang/css.vim'
	Plugin 'cakebaker/scss-syntax.vim'
	Plugin 'godlygeek/tabular'
	Plugin 'plasticboy/vim-markdown'
	Plugin 'elzr/vim-json'
	Plugin 'stephpy/vim-yaml'

	" plugin from http://vim-scripts.org/vim/scripts.html
	" Plugin 'L9'

call vundle#end()            " required
"-----------------------------------------------------

" Set some keys for Haskell type checking
noremap <Leader>ht :GhcModType<cr>
nnoremap <Leader>htc :GhcModTypeClear<cr>

