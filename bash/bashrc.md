# Bash

Configuration for bash.

Let's save it in file bashrc.symlink

[bashrc.symlink](#Structure "save:")

## Structure
    _"Configuration"
    _"Path"
    _"Aliases"
    _"Prompt"
    _"Misc"

## Configuration
If not running interactively, don't do anything

    case $- in
        *i*) ;;
        *) return;;
    esac

## Path

First we add a small function for clean PATH inclusion:

    add_to_path(){
        new=$1
        case ":${PATH:=$new}:" in
            *:$new:*)  ;;
            *) PATH="$new:$PATH"  ;;
        esac
    }

and then we include some directories:

    add_to_path "${HOME}/opt/nodejs/bin"
    add_to_path "${HOME}/opt/julia"

## Aliases
Turn on colors on `ls`

    alias ls='ls --color=auto'
    eval `dircolors /home/santos/.dircolors-solarized/dircolors.256dark`

Simple archiver

    alias ae='aunpack' # Must have atools installed!

Fancy Git log tree

    alias glog="git log --graph --oneline --decorate --date-order --color --boundary"


## Prompt
Git in bash
Command completion
If you’re a Bash user, you can tap into some of your shell’s features to make your experience with Git a lot friendlier. Git actually ships with plugins for several shells, but it’s not turned on by default.

First, you need to get a copy of the contrib/completion/git-completion.bash file out of the Git source code. Copy it somewhere handy, like your home directory, and add this to your .bashrc:

http://git-scm.com/book/en/v2/Git-in-Other-Environments-Git-in-Bash
source /usr/share/git/completion/git-completion.bash

Change prompt

    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWSTASHSTATE=1
    export GIT_PS1_SHOWUNTRACKEDFILES=1
    export GIT_PS1_SHOWUPSTREAM=1
    source /usr/share/git/git-prompt.sh

    export PS1='$(git branch &>/dev/null;\
    if [ $? -eq 0 ]; then \
        echo "$(echo `git status` | grep "nothing to commit" > /dev/null 2>&1; \
        if [ "$?" -eq "0" ]; then \
            # @4 - Clean repository - nothing to commit
            echo "'$Green'"$(__git_ps1 " (%s)"); \
        else \
            # @5 - Changes to working tree
            echo "'$Red'"$(__git_ps1 " {%s}"); \
        fi) '$Yellow$PathShort$Color_Off' >> "; \
    else \
        # @2 - Prompt when not in GIT repo
        echo " '$Yellow$PathShort$Color_Off' >> "; \
    fi)'

## Misc
Colored `man`

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

