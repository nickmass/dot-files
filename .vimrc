
" Bundle Scripts-----------------------------
if has('vim_starting')
  set nocompatible               " Be iMproved

  " Required:
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('~/.vim/bundle'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" My Bundles Here:
NeoBundle 'chriskempson/base16-vim'
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'rust-lang/rust.vim'
NeoBundle 'Valloric/YouCompleteMe'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'ebfe/vim-racer'

" Required:
call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"End NeoBundle Scripts-------------------------

let g:ycm_key_list_select_completion = ['<TAB>']
let g:ycm_key_list_previous_completion = ['<S-TAB>']

let g:gitgutter_override_sign_column_highlight = 0

imap jk <esc>
vmap jk <esc>
nmap <S-tab> <<
imap <S-tab> <esc><<i

command W w !sudo tee %

set nonumber
set tabstop=4
set shiftwidth=4
set autoindent
set history=1000
set undolevels=1000
set nobackup
set noswapfile

let base16colorspace=256
set background=dark
colorscheme base16-default

syntax on

highlight DiffAdd    	cterm=bold ctermbg=17
highlight DiffDelete	cterm=bold ctermbg=88
highlight DiffChange 	cterm=bold ctermbg=56
highlight DiffText   	cterm=bold ctermbg=237
highlight Normal 		ctermbg=0
highlight NonText 		ctermbg=0 ctermfg=0
highlight LineNr		ctermbg=0
highlight SignColumn	ctermbg=0
highlight GitGutterAdd 	ctermbg=0
highlight GitGutterDelete 		ctermbg=0
highlight GitGutterChange 		ctermbg=0
highlight GitGutterChangeDelete ctermbg=0
