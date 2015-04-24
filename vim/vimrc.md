# Vim
[:tangle nvimrc.symlink]

Because I was a vimmer sometime ago, and I keep using it when I forget to change the appropiate environment variable.

# Environment
```
set t_Co=256        " Enable 256 colors
set number
sy on
colorscheme jellybeans " Choose colorscheme
set mouse=a " Enable mouse on all modes
set history=1000   " Sets how many lines of history VIM has to remember
```

## Textual search
```
set ignorecase    " Case-insensitive search
set smartcase     " With this option, a search is case-insensitive if
                  " you enter the search string in ALL lower case
set hlsearch      " Highlight search
set incsearch     " Incremental search
```

## Text formatting
```
set wrap linebreak nolist " do soft word wrap
"set encoding=utf8 " Set utf8 as standard encoding and en_US as the standard language
```

# Misc settings
```
" No noise from VIM! {
    set noerrorbells
    " Hides buffers instead of close them
    set hidden
" }
set autoread       " Set to auto read when a file is changed from the outside

au BufNewFile,BufRead *.jl,*.julia set filetype=julia
filetype plugin on " Enable filetype plugin
filetype indent on
```

# Statusline
```
" Always display the statusline
set laststatus=2
```

