#
# Prompt configuration
#
# Prompt is configured throught the =$PS1= environment variable.
#

################################################################################
# Colors
#
# Besides setting the text color, we can set a background color. It can be
# achieved by just passing a specific code. Nonetheless, note that if we want to
# set both text color and background color, we can type two codes in one place,
# separating them by semi-colon (;).
#
# A sequence that applies color is as follows:
#
#     \[\e[Nm\]
#
# where N is a sequence of type a;b
#     a - foreground color
#     b - background color
#
# Check the colors within the following table:
#
#    | Color          | Foreground | Background |
#    |----------------+------------+------------|
#    | default        |         39 |         49 |
#    | black          |         30 |         40 |
#    | red            |         31 |         41 |
#    | green          |         32 |         42 |
#    | yellow         |         33 |         43 |
#    | blue           |         34 |         44 |
#    | magenta        |         35 |         45 |
#    | cyan           |         36 |         46 |
#    | white          |         97 |        107 |
#    | bright gray    |         37 |         47 |
#    | dark gray      |         90 |        100 |
#    | bright red     |         91 |        101 |
#    | bright green   |         92 |        102 |
#    | bright yellow  |         93 |        103 |
#    | bright blue    |         94 |        104 |
#    | bright magenta |         95 |        105 |
#    | bright cyan    |         96 |        106 |
#

################################################################################
# Set git info: keep it at GIT_PS1_INFO
#
# Default if you have Git installed. I'm not sure if this path is default across
# different distros
if [[ -f "/usr/share/git/git-prompt.sh" ]]
then
    source "/usr/share/git/git-prompt.sh"

    # Configure `__git_ps1` to tell us as much as possible
    export GIT_PS1_SHOWDIRTYSTATE=1 GIT_PS1_SHOWSTASHSTATE=1 GIT_PS1_SHOWUNTRACKEDFILES=1
    export GIT_PS1_SHOWUPSTREAM=verbose GIT_PS1_DESCRIBE_STYLE=branch GIT_PS1_SHOWCOLORHINTS=1
    export GIT_PS1_HIDE_IF_PWD_IGNORED=1
    export GIT_PS1_INFO='$(__git_ps1 "[ %s ]")'

    # Unrelated but useful: avoid auto-edit on successful merges, starting with Git 2.0
    export GIT_MERGE_AUTOEDIT=no
else
    export GIT_PS1_INFO=''
fi

# This variable controls the number of trailing directory components to retain when expanding \w. The removed characters are replaced with ellipsis.
PROMPT_DIRTRIM=2
PS1="\`if [ \$? = 0 ]; then echo \[\e[32m\]*\[\e[0m\]; else echo \[\e[31m\]!\[\e[0m\]; fi\` \t \u@\h:\[\e[34m\]\w\[\e[0m\] \[\e[33m\]${GIT_PS1_INFO}\[\e[0m\]\$ "
