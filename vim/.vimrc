"------------------------------------------
" Author: Kurt Lachmann
"------------------------------------------


"------------------------------------------------------------
"   Plugins (Vundle)
"------------------------------------------------------------

" Turn stuff off so that Vundle works:
set nocompatible
filetype off


" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'


" Automatic closing of quotes, parantheses, brackets, ...
Plugin 'delimitMate.vim'
" Enable expansion behaviour as known from modern IDEs:
let delimitMate_expand_cr = 1


" NERDTree - for file browsing
Plugin 'scrooloose/nerdtree'


" Manipulate surroundings:
" cs({  change surrounds from () to {}
" ds"   remove surrounding "
" ysiw" add " around a word
Plugin 'surround.vim'


" Comment out stuff
" gcc   comment a line
" gc    comment the target of a motion
" gc    in visual mode comment the selection
Plugin 'tpope/vim-commentary'


" Make plugins repeatable
Plugin 'tpope/vim-repeat'


" Adds new movement ,w ,b ,e ...
" to move over camelCaseVariables or variables_with_underscores
Plugin 'chaoren/vim-wordmotion'
" The more popular 'CamelCaseMotion' had some unwanted behaviour for ciw
" See the issues at the github for:
" Plugin 'bkad/CamelCaseMotion'


" New textobjects for function arguments:
" cia   change inside argument
" daa   deletes a whole argument
" To be used when calling a function, e.g.:
" int x = foo(arg1, arg2[x], arg3.foobar);
Plugin 'argtextobj.vim'


" Visually appealing statusline
Plugin 'itchyny/lightline.vim'


" Enable stuff again that was turned off for Vundle:
call vundle#end()
filetype plugin indent on
"------------------------------------------
"                End Vundle
"------------------------------------------

" To enable the Ctrl-X completion menu again.
" Seems to be disabled by YouCompleteMe.
" autocmd VimEnter set shortmess-=c

" set omnifunc=syntaxcomplete#Complete



"------------------------------------------------------------
"   Colorscheme
"------------------------------------------------------------

set background=dark
syntax on
set cursorline
colorscheme koehler

"   Mappings
"------------------------------------------------------------


" Spacebar as leader
let mapleader = "\<Space>"


" Leader-b for file browsing
nmap <Leader>b :NERDTreeToggle<CR>


" Leader-o to switch to opened buffers
nnoremap <leader>o :ls<cr>:b<space>


" Leader-e to switch to the last edited buffer
nnoremap <leader>e :b#<CR>


" Insert a new line without entering insert mode.
" Enter        - insert below current line
" Leader-Enter - insert above current line
nnoremap <silent> <Leader><CR> :pu!_<Enter>
nnoremap <silent> <CR> :pu_<Enter>


" Save file with Leader-w
nnoremap <Leader>w :w<CR>


" Close window with Leader-q
nnoremap <Leader>q :q<CR>


" Execute the current file with Leader-r
" (%:p refers to the file's full path)
"nnoremap <Leader>r :!%:p<CR>
autocmd FileType python nnoremap <Leader>r :w<CR>:exec '!python' shellescape(@%, 1)<CR>


" Append a semicolon to end of line with Leader-Comma
" WARNING: overrides marker z
nnoremap <Leader>, mzA;<Esc>`z


" More intuitive indentation of visually selected text
vnoremap < <gv
vnoremap > >gv 


" Copy/paste to or from system clipboard with typically used shortcuts
vnoremap <C-c> "+y
inoremap <C-v> <C-r>+


" Toggle search highlighting on and off
map <silent> <Leader>h :set hlsearch!<CR>


" Always have the cursor centered on the screen
" after jumping a (half-) page up/down
nnoremap <C-B> <C-B>zz
nnoremap <C-F> <C-F>zz
nnoremap <C-U> <C-U>zz
nnoremap <C-D> <C-D>zz

" Do the same when searching
nnoremap n nzz
nnoremap N Nzz


" Continue last f,F,t,T in the same direction with , and
" in the opposite direction with ;
" Essentially swapping the default behaviour.
nnoremap , ;
nnoremap ; ,


" Increment and decrement numbers with + and -
nnoremap + <C-a>
nnoremap - <C-x>


" Map Y to act like D and C, i.e. to yank until EOL, rather than act as yy,
" which is the default
map Y y$
 




"------------------------------------------------------------
"   Custom behaviour
"------------------------------------------------------------

" Set 'nocompatible' to ward off unexpected things that your distro might
" have made, as well as sanely reset options when re-sourcing .vimrc
set nocompatible

set hidden

" Better command-line completion
set wildmenu
 
" Show partial commands in the last line of the screen
set showcmd

" Case insensitive file completion
set wildignorecase

" Use case insensitive search, except when using capital letters
set ignorecase
set smartcase
 
" Allow backspacing over autoindent, line breaks and start of insert action
set backspace=indent,eol,start
 
" When opening a new line and no filetype-specific indenting is enabled, keep
" the same indent as the line you're currently on. Useful for READMEs, etc.
set autoindent
 
" Stop certain movements from always going to the first character of a line.
" While this behaviour deviates from that of Vi, it does what most users
" coming from other editors would expect.
set nostartofline
 
" Attempt to determine the type of a file based on its name and possibly its
" contents. Use this to allow intelligent auto-indenting for each filetype,
" and for plugins that are filetype specific.
filetype indent plugin on

" Instead of failing a command because of unsaved changes, instead raise a
" dialogue asking if you wish to save changed files.
set confirm
 
" Use visual bell instead of beeping when doing something wrong
set visualbell
 
" And reset the terminal code for the visual bell. If visualbell is set, and
" this line is also included, vim will neither flash nor beep. If visualbell
" is unset, this does nothing.
set t_vb=
 
" Enable use of the mouse for all modes
set mouse=a
 
" Set the command window height to 2 lines, to avoid many cases of having to
" "press <Enter> to continue"
set cmdheight=2
 
" Display line numbers on the left
set number
set relativenumber
 
" Indentation settings for using hard tabs for indent. Display tabs as
" four characters wide.
set shiftwidth=4
set tabstop=4
set softtabstop=4
set noexpandtab

" Place the cursor on the left side of a tab character (default is rightmost column)
set list listchars=tab:\ \ 

" Quickly time out on keycodes, but never time out on mappings
set notimeout ttimeout ttimeoutlen=200
 
" Use <F11> to toggle between 'paste' and 'nopaste'
set pastetoggle=<F11>

" Display the cursor position on the last line of the screen or in the status
" line of a window
set ruler
 
" Always display the status line, even if only one window is displayed
set laststatus=2

" Show search matches as you type 
set incsearch

" Always show some lines above/below the current line
set scrolloff=5

" New windows should be opened below or right
set splitbelow splitright

" Wrap at words
set linebreak



" When lines are wrapped move up/down in displayed lines
" not in actual lines.
" ONLY FOR QWERTY
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
nnoremap <Down> gj
nnoremap <Up> gk
vnoremap <Down> gj
vnoremap <Up> gk
inoremap <Down> <C-o>gj
inoremap <Up> <C-o>gk

" Colemak customization

" Change movement for colemak
" set langmap=hj,jh,kl,lk,HJ,JH,KL,LK
" set langnoremap

" Adjusted for colemak
" The langmap is applied to everything.
" That means e.g. (nnoremap j gh) is translated into (nnoremap h gj)
" nnoremap j gh
" nnoremap k gl
" vnoremap j gh
" vnoremap k gl
" nnoremap <Down> gh
" nnoremap <Up> gl
" vnoremap <Down> gh
" vnoremap <Up> gl
" inoremap <Down> <C-o>gh
