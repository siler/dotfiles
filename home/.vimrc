"Configure Vundle
set nocompatible
filetype off

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

" Vundle can manage Vundle!
Bundle 'gmarik/vundle'

" Vundle can install these too:
Bundle 'tpope/vim-sensible'

" Pretty colors
Bundle 'tomasr/molokai'

" airline status bar
Bundle 'bling/vim-airline'

" Snippets
Bundle "SirVer/ultisnips.git"

" File and contents search
Bundle 'kien/ctrlp.vim'
Bundle 'mileszs/ack.vim'

" Undo tree tool
Bundle "sjl/gundo.vim"

" Excellent tpope stuff
Bundle 'tpope/vim-eunuch'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-sleuth'

" Tig in vim
Bundle 'int3/vim-extradite'

" Markdown
Bundle 'plasticboy/vim-markdown'

" JS stuff
Bundle 'jelera/vim-javascript-syntax'
Bundle 'pangloss/vim-javascript'
Bundle 'kchmck/vim-coffee-script'
Bundle 'elzr/vim-json'

" Syntax stuff + python additions
Bundle 'scrooloose/syntastic'
Bundle 'nvie/vim-flake8'
Bundle 'hynek/vim-python-pep8-indent'

" Better Go complete
Bundle 'Blackrush/vim-gocode'

" Debugging
Bundle 'joonty/vdebug.git'

filetype plugin indent on

" Do not need to show the mode on the status line, powerline handles this
set noshowmode

" Show typed command in lower right if incomplete
set showcmd

" general tabstop settings
set smarttab
set shiftwidth=3
set tabstop=3
set noexpandtab

" Specific tabstop settings:
augroup Indentation
	autocmd!
	autocmd FileType c setlocal tabstop=4 shiftwidth=4
	autocmd FileType coffee setlocal shiftwidth=2 softtabstop=2 expandtab
	autocmd FileType javascript setlocal shiftwidth=2 softtabstop=2 expandtab
	autocmd FileType haskell setlocal shiftwidth=2 softtabstop=2 expandtab
	autocmd FileType python setlocal shiftwidth=4 softtabstop=4 expandtab textwidth=79
augroup END

" show whitespace
set list
let &listchars = "tab:\u2192\ ,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u00b7"

" remove horrible split characters
set fillchars+=vert:\ 

" keep undo files in a specific location instead of littering code trees
set undodir=~/.cache/vim

" Color configuration
set t_Co=256
colorscheme molokai

" Configure fast escaping, otherwise it waits and is silly
augroup FastEscape
	autocmd!
	autocmd InsertEnter * set timeoutlen=0
	autocmd InsertLeave * set timeoutlen=1000
augroup END

" Save these in viminfo
set viminfo='100,h

set history=100

set wildmode=list:longest,full
set wildignore+=*/tmp/*,*.so,*.swp,*/tests/coverage/*
let g:ctrlp_custom_ignore = {
			\ 'dir':  '\v[\/]\.(tests/coverage)$',
			\ 'file': '\v\.(png|jpg|swf)$',
			\ }

" Set substitutions to always apply globally to lines
set gdefault

" Use case insensitive searches when searching with all lowercase but case
" sensitive if at least one letter is capitalized
set ignorecase
set smartcase

" Use relative line numbers for the win
set relativenumber

" Set leader key to ,
let mapleader = "\<space>"

" Set 'normal' regex syntax (see :magic)
nnoremap / /\v
vnoremap / /\v

" Create a sequence to clear search results
nnoremap <leader><space> :noh<cr>

" Strip whitespace from file
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<cr>

" Search commands
nnoremap <leader>a :Ack<space>
nnoremap <leader>g :Ggrep<space>
nnoremap <leader>fw :call FindWord()<cr>
nnoremap <leader>ff :call FindFunc()<cr>

function! FindWord()
	let l:Word = expand("<cword>")
	execute ":Ggrep ".l:Word
endfunction

function! FindFunc()
	let l:Word = expand("<cword>")
	execute ":Ggrep 'function ".l:Word."'"
endfunction

" Allow saving of files as sudo when I forgot to start vim using sudo.
cnoremap W!! w !sudo tee > /dev/null %

" Remap ctrl-space to omnicomplete
inoremap <C-@> <C-x><C-o>

" Editing and loading of vimrc
nnoremap <silent> <leader>ev :edit $MYVIMRC<cr>
nnoremap <silent> <leader>sv :source $MYVIMRC<cr>
nnoremap <silent> <leader>es :UltiSnipsEdit<cr>

" Common command mistakes
command! W w
command! Q q

augroup MakeCoffee
	autocmd!
	"autocmd BufWritePost *.coffee silent make!
augroup END

" Window management hotkeys
function! WinMove(key)
	let t:curwin = winnr()
	execute "wincmd ".a:key

	if (t:curwin == winnr()) " Couldn't move, create a window
		if (match(a:key,'[jk]')) " Create vertical split
			wincmd v
		else " Create horizontal split
			wincmd s
		endif

		" Move to the newly created window
		exec "wincmd ".a:key
	endif
endfunction

" Window navigate/create
noremap <leader>h :call WinMove('h')<cr>
noremap <leader>k :call WinMove('k')<cr>
noremap <leader>l :call WinMove('l')<cr>
noremap <leader>j :call WinMove('j')<cr>

" Window moving
noremap <leader>H :wincmd H<cr>
noremap <leader>K :wincmd K<cr>
noremap <leader>L :wincmd L<cr>
noremap <leader>J :wincmd J<cr>

" Window resizing
nnoremap <left>  :3wincmd <<cr>
nnoremap <right> :3wincmd ><cr>
nnoremap <up>    :3wincmd +<cr>
nnoremap <down>  :3wincmd -<cr>

" Close/rotate
noremap <leader>wc :wincmd q<cr>
noremap <leader>wr <C-W>r

" Ack default command
let g:ackprg="ack -H --nocolor --nogroup --column"

" CtrlP working directory
let g:ctrlp_working_path_mode="wra"

" CtrlP find
nnoremap <leader>f :CtrlPMRUFiles<cr>
nnoremap <leader>b :CtrlPBuffer<cr>

" Ultisnips configuraiton
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
let g:UltiSnipsSnippetsDir = "~/.vim/snippets"
let g:UltiSnipsSnippetDirectories = ["snippets"]

" no more silly json double quote hiding
let g:vim_json_syntax_conceal = 0

" Set up some sweet powerline features
let g:airline_left_sep = ''
let g:airline_right_sep = ''

if !exists('g:airline_symbols')
	let g:airline_symbols = {}
endif

let g:airline_symbols.linenr = '␤'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.whitespace = 'Ξ'

let g:syntastic_php_phpcs_args = "--tab-width=4 --standard=Barracuda"

let g:vdebug_options = {
			\ 'break_on_open' : 0,
			\ 'continuous_mode' : 1
			\}

set completeopt+=longest
