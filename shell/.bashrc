#
# Configuration for non-login shell
#

CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}"
BASH_CONFIG_DIR="${CONFIG_DIR}/bash"

# Load configuration files
for file in ${BASH_CONFIG_DIR}/*
do 
    source "$file"
done

# Load specific ranger configuration (take a look at the package =ranger= within
# the dotfiles
if [[ -f "${HOME}/.ranger_automatic_cd.sh" ]]
then
    source "${HOME}/.ranger_automatic_cd.sh"
fi

# Load dircolors
if [[ -f "${HOME}/.dir_colors" ]]
then
    eval $(dircolors "${HOME}/.dir_colors")
fi

# Additional configuration can be done by adding them to =.bash.local=, which is
# loaded at the very end of this file. This file is supposed to keep very
# specific configuration which is not versioned.
if [[ -f "${HOME}/.bash.local" ]]
then
    source "${HOME}/.bash.local"
fi
