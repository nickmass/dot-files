if &compatible
	set nocompatible
endif
set runtimepath^=/home/nickmass/.vim/repos/github.com/Shougo/dein.vim

call dein#begin('/home/nickmass/.vim')
" My Bundles Here:
call dein#add('Shougo/dein.vim')
call dein#add('Shougo/vimproc.vim', {'build': 'make'})
call dein#add('chriskempson/base16-vim')
call dein#add('Shougo/unite.vim')
call dein#add('rust-lang/rust.vim')
call dein#add('airblade/vim-gitgutter')
call dein#add('racer-rust/vim-racer')
call dein#add('scrooloose/syntastic')
call dein#add('tpope/vim-unimpaired')
call dein#end()

if dein#check_install()
	call dein#install()
endif

filetype plugin indent on

let g:racer_cmd = "racer"
let g:rustfmt_autosave = 0
let g:rustfmt_fail_silently = 1
let g:gitgutter_override_sign_column_highlight = 0

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

call unite#custom#profile('default', 'context', {
			\ 'start_insert': 1
			\})

nmap <S-tab> <<
imap <S-tab> <esc><<i
nmap <C-p> :Unite file_rec/async buffer<CR>
imap <C-p> <esc>:Unite file_rec/async buffer<CR>

command W w !sudo tee %

set hidden
set number
set tabstop=4
set shiftwidth=4
set autoindent
set history=1000
set undolevels=1000
set nobackup
set noswapfile

let base16colorspace=256
set background=dark
colorscheme base16-default-dark

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
