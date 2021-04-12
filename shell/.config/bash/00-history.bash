#
# Bash history configuration
#
# Bash has command history support that allows you to recall previously run
# commands and run them again at a later session.
#

################################################################################
# Fix history file
#
# Command history is stored both in memory and in a special file written to
# disk, stored in =HISTFILE=:
HISTFILE="${HOME}/.bash_history"

################################################################################
# Save all commands within history
#
# To prevent loss of command history, use the option =histappend= to append to
# the history instead of overwriting the previous session:
shopt -s histappend
# We also tell Bash to try saving multi-line commands in the same entry, when
# possible:
shopt -s cmdhist

################################################################################
# Share history across different sessions
#
# =PROMPT_COMMAND= variable specifies commands to be executed prior issuing each
# primary prompt. We use it to append and reload BASH history, so that we can
# acess commands from different sessions (useful within tmux):
export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$"\n"}history -a; history -c; history -r"

################################################################################
# Commands to ignore in history
#
# I also ignore too short commands, like =ls= and =pwd=. This is only junk in
# the history, as they are simple and fast to type.
HISTIGNORE="?:??:???:${HISTIGNORE}"
# When saving command history in memory, I want to prevent two things from being
# added:
#
#    - =ignorespace= :: lines beginning with whitespace (in case we have a
#      reason to run a command and not remember it);
#    - =ignoredups= :: duplicate lines (which are just a nuisance to scroll
#      through).
#    - =erasedups= :: causes all previous lines matching the current line to be
#      removed from the history list before that line is saved.
#
# We don't want this environment variable to leak into subshells (especially
# noninteractive subshells), so we don't =export= it.
HISTCONTROL=ignorespace:ignoredups:erasedups

################################################################################
# Unlimited history
#
# I also like to keep an unlimited history list (see =info bash=)
HISTSIZE=-1
HISTFILESIZE=-1

################################################################################
# Set timestamp
#
# The =HISTTIMEFORMAT= variable allows the inclusion of a timestamp on each
# command entry in the history file. It uses the same format string method as
# the strftime command.
HISTTIMEFORMAT='%F %T '

################################################################################
# Other settings
#
# Allows a user to re-edit a failed history substitution instead of clearing the
# prompt.
shopt -s histreedit
# Results of history are not immediately passed to the shell parser. The
# resulting line is loaded into the Readline editing buffer, allowing
# modifications.
shopt -s histverify
