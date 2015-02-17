# Zsh configuration

[:tangle zshrc.symlink]

# PATH configuration
```sh
typeset -gU path

#=============
# '0.sandbox'
#
# Unprocessed items that don’t have a place yet in here. It is a temporary
# folder for files you're messing around with but don't need to save long-term.
# This may be items like software installers you’ve downloaded, files sent to
# you from colleagues, random text clippings and testing scripts. Files I
# decide I want to keep graduate from '0.sandbox' to '1.docs'. This folder must
# be emptied everyday. To avoid procrastination, resulting in a completely
# cluttered folder, you can keep a 'janitor' script, deleting its contents
# regularly.
SANDBOX=$HOME/0.sandbox/

#=============
# '1.docs'
#
# Is the big kahuna(*) of all the six folders. It's the place where all the
# working files for your currently in-progress tasks, projects and clients
# go. Each project gets its own unique folder. You may have many sub-folders in
# this folder, but don't go deeper than 3 subfolders. This directory changes
# often and frequently, so you must clean it regularly. Because of this most of
# the files here are kept under github/gitlab control.
#
# (*) Kahuna is a Hawaiian word, defined in Pukui & Elbert (1986) as a "priest,
# sorcerer, magician, wizard, minister, expert in any profession". (See also
# Ancient Hawaii.) Forty types of kahuna are listed in the book Tales from the
# Night Rainbow.
DOCS=$HOME/1.docs/

#=============
# '2.archive'
#
# Completed projects, general reference items, and anything else which is
# important and you might want to look at again go here. From here you can
# create aliases for the default folder from your system, like 'Books' or
# 'Pictures'.  The files here don't change much if ever, and so you can back
# them up on a different (less frequent) schedule. Therefore I elected this
# folder to be actually a symbolic link to my Dropbox folder.
ARCHIVE=$HOME/2.archive/

#=============
# 'opt'
#
# Third-party software, not present among the official packages.
OPT=$HOME/opt

pathdirs=(
$OPT/julia
$OPT/nodejs/bin
$OPT/atom-editor/bin
$OPT/tw5
)
path=($path $pathdirs)
```

## Oh-my-zsh
### Path to oh-my-zsh configuration
```sh
ZSH=$HOME/.oh-my-zsh
```

### Theme
```sh
ZSH_THEME="agnoster"
```

### Plugins
```sh
plugins=(git archlinux tmux)
```

### Load oh-my-zsh
```sh
source $ZSH/oh-my-zsh.sh
```

# General configuration
## History
```sh
export HISTSIZE=10000
export SAVEHIST=10000
export HISTFILE=~/.zsh_history
```

## Aliases
Load custom aliases
```sh
[ -e "${HOME}/.zsh_aliases" ] && source "${HOME}/.zsh_aliases"
```

```sh
alias ae='aunpack' # Must have atools installed!
alias df="df -H"
alias du="du -ch"
alias ls="ls --color=auto"
alias glog="git log --graph --oneline --decorate --date-order --color --boundary"
alias lh='ls -l .??*'
alias lhd='ls -ld .??*'

man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
            man "$@"
}
```

## Plugins
Solves GREP problem
```sh
alias grep="/usr/bin/grep $GREP_OPTIONS"
unset GREP_OPTIONS
```

Preferred editor for local and remote sessions
```sh
if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='vi'
else
   export EDITOR='vim'
fi
```

```sh
export ARCHFLAGS="-arch x86_64"
setopt HIST_IGNORE_DUPS
```

TeXlive does not allow me to run bibtex
on /tmp; to avoid this, set this variable
```sh
export openout_any=a
```

Show only past commands beginning with the current input
```sh
[[ -n "${key[PageUp]}"   ]]  && bindkey  "${key[PageUp]}"    history-beginning-search-backward
[[ -n "${key[PageDown]}" ]]  && bindkey  "${key[PageDown]}"  history-beginning-search-forward
```

List packages installed explicitly by the user
```sh
lspacuser() {
    pacman -Qei | awk '/^Name/ { name=$3 } /^Groups/ { if ( $3 != "base" && $3 != "base-devel" ) { print name } }'
}
```

List packages installed from AUR
```sh
lspacaur() {
   pacman -Qqm
}
```

colored ls
```sh
eval `dircolors /home/santos/.dircolors-solarized/dircolors.256dark`
```

Command completion
```sh
autoload -U compinit
compinit
```

Uncomment the following line to use case-sensitive completion.
```sh
CASE_SENSITIVE="true"
```