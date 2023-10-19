set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath

syntax off
map <F4> :only <CR>
map <F1> :help <CR> <F4>

require'lspconfig'.pyright.setup{}
