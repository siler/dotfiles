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

" Ability to kill buffer without closing window
Bundle 'vim-scripts/bufkill.vim'

" Alignment tool
Bundle 'vim-scripts/Align'

" SQL tools
Bundle 'vim-scripts/SQLUtilities'

" JS stuff
Bundle 'jelera/vim-javascript-syntax'
Bundle 'pangloss/vim-javascript'
Bundle 'kchmck/vim-coffee-script'

" Syntax stuff + python additions
Bundle 'scrooloose/syntastic'
Bundle 'nvie/vim-flake8'
Bundle 'hynek/vim-python-pep8-indent'

" Better PHP Complete
Bundle 'shawncplus/phpcomplete.vim'

" Debugging
Bundle 'joonty/vdebug.git'

" Supertab!
Bundle 'ervandew/supertab'

" Color info
Bundle 'vim-scripts/Colortest'

filetype plugin indent on

" Prevent a couple of vulnerabilities
set modelines=0

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
	autocmd FileType coffee setlocal shiftwidth=2 softtabstop=2 expandtab
	autocmd FileType javascript setlocal shiftwidth=2 softtabstop=2 expandtab
	autocmd FileType haskell setlocal shiftwidth=2 softtabstop=2 expandtab
	autocmd FileType python setlocal shiftwidth=4 softtabstop=4 expandtab textwidth=79
augroup END

" show whitespace
set list
let &listchars = "tab:\u279e\ ,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u00b7"

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
set wildignore+=*/tmp/*,*.so,*.swp,*/vendor/*

" Set substitutions to always apply globally to lines
set gdefault

" Use case insensitive searches when searching with all lowercase but case
" sensitive if at least one letter is capitalized
set ignorecase
set smartcase

" Use relative line numbers for the win
set relativenumber

" Set leader key to ,
let mapleader = ","

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
cnoremap W!! %!sudo tee > /dev/null %

" Remap ctrl-space to omnicomplete
inoremap <C-@> <C-x><C-o>

" Editing and loading of vimrc
nnoremap <silent> <leader>ev :edit $MYVIMRC<cr>
nnoremap <silent> <leader>sv :source $MYVIMRC<cr>
nnoremap <silent> <leader>es :UltiSnipsEdit<cr>

" apply gofmt to the current file silently
function! GoFormat()
	let l:cursor_pos = getpos('.')
	silent %!gofmt
	call setpos('.', l:cursor_pos)
endfunction

augroup FormatGo
	autocmd!
	autocmd BufWritePre *.go :call GoFormat()
augroup END

augroup MakeCoffee
	autocmd!
	autocmd BufWritePost *.coffee silent make!
augroup END

" Window management hotkeys
function! WinMove(key)
	let t:curwin = winnr()

	execute "wincmd ".a:key

	" Couldn't move, create a window
	if (t:curwin == winnr())

		" Create vertical split
		if (match(a:key,'[jk]'))
			wincmd v

		" Create horizontal split
		else
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
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsSnippetsDir="~/.vim/snippets"
let g:UltiSnipsSnippetDirectories=["snippets"]

"SQLUtil keyword case update
let g:sqlutil_keyword_case = '\U'
let g:sqlutil_align_where = 0

"phpcomplete awesomeness
let g:phpcomplete_parse_docblock_comments = 1

"supertab configuration
let g:SuperTabDefaultCompletionType = "<C-x><C-o>"

let g:airline_powerline_fonts = 1

set completeopt+=longest
