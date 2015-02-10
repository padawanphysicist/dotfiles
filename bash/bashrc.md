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

    add_to_path "${HOME}/.cabal/bin" 
    add_to_path "${HOME}/.local/bin" 
    add_to_path "${HOME}/opt/nodejs/bin"
    add_to_path "${HOME}/opt/julia"

## Aliases
Turn on colors on `ls`

    alias ls='ls --color=auto'
    eval `dircolors /home/santos/.dircolors-solarized/dircolors.256dark`

Syntatic sugar for bash colors
    # Reset
    Color_Off='\e[0m'       # Text Reset
    
    # Regular Colors
    Black='\e[0;30m'        # Black
    Red='\e[0;31m'          # Red
    Green='\e[0;32m'        # Green
    Yellow='\e[0;33m'       # Yellow
    Blue='\e[0;34m'         # Blue
    Purple='\e[0;35m'       # Purple
    Cyan='\e[0;36m'         # Cyan
    White='\e[0;37m'        # White
    
    # Bold
    BBlack='\e[1;30m'       # Black
    BRed='\e[1;31m'         # Red
    BGreen='\e[1;32m'       # Green
    BYellow='\e[1;33m'      # Yellow
    BBlue='\e[1;34m'        # Blue
    BPurple='\e[1;35m'      # Purple
    BCyan='\e[1;36m'        # Cyan
    BWhite='\e[1;37m'       # White
    
    # Underline
    UBlack='\e[4;30m'       # Black
    URed='\e[4;31m'         # Red
    UGreen='\e[4;32m'       # Green
    UYellow='\e[4;33m'      # Yellow
    UBlue='\e[4;34m'        # Blue
    UPurple='\e[4;35m'      # Purple
    UCyan='\e[4;36m'        # Cyan
    UWhite='\e[4;37m'       # White
    
    # Background
    On_Black='\e[40m'       # Black
    On_Red='\e[41m'         # Red
    On_Green='\e[42m'       # Green
    On_Yellow='\e[43m'      # Yellow
    On_Blue='\e[44m'        # Blue
    On_Purple='\e[45m'      # Purple
    On_Cyan='\e[46m'        # Cyan
    On_White='\e[47m'       # White
    
    # High Intensity
    IBlack='\e[0;90m'       # Black
    IRed='\e[0;91m'         # Red
    IGreen='\e[0;92m'       # Green
    IYellow='\e[0;93m'      # Yellow
    IBlue='\e[0;94m'        # Blue
    IPurple='\e[0;95m'      # Purple
    ICyan='\e[0;96m'        # Cyan
    IWhite='\e[0;97m'       # White
    
    # Bold High Intensity
    BIBlack='\e[1;90m'      # Black
    BIRed='\e[1;91m'        # Red
    BIGreen='\e[1;92m'      # Green
    BIYellow='\e[1;93m'     # Yellow
    BIBlue='\e[1;94m'       # Blue
    BIPurple='\e[1;95m'     # Purple
    BICyan='\e[1;96m'       # Cyan
    BIWhite='\e[1;97m'      # White
    
    # High Intensity backgrounds
    On_IBlack='\e[0;100m'   # Black
    On_IRed='\e[0;101m'     # Red
    On_IGreen='\e[0;102m'   # Green
    On_IYellow='\e[0;103m'  # Yellow
    On_IBlue='\e[0;104m'    # Blue
    On_IPurple='\e[0;105m'  # Purple
    On_ICyan='\e[0;106m'    # Cyan
    On_IWhite='\e[0;107m'   # White

    # Various variables you might want for your PS1 prompt instead
    Time12h="\T"
    Time12a="\@"
    PathShort="\w"
    PathFull="\W"
    NewLine="\n"
    Jobs="\j"

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

