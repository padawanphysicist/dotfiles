#
# Pager configuration
#
# Due to its speed and simplicity, GNU less is probably the most common default
# terminal pager on various GNU/Linux distributions—you may have probably used
# it explicitly via the less command, or implicitly when you execute the man
# command or git diff. Although the default configuration of less does not
# really offer much except for a basic text viewer, it is actually much more
# powerful than most people think. Here a few improvements over the default
# configuration are offered.
#

export LESS='--quit-if-one-screen --ignore-case --status-column --LONG-PROMPT --RAW-CONTROL-CHARS --HILITE-UNREAD --tabs=4 --no-init --window=-4 --chop-long-lines'

# Set colors for less. Borrowed from https://wiki.archlinux.org/index.php/Color_output_in_console#less .
export LESS_TERMCAP_mb=$'\E[1;31m'     # begin bold
export LESS_TERMCAP_md=$'\E[1;36m'     # begin blink
export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin reverse video
export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
export LESS_TERMCAP_us=$'\E[1;32m'     # begin underline
export LESS_TERMCAP_ue=$'\E[0m'        # reset underline

