# Read this number of lines into history buffer on startup.
export HISTSIZE=1000000

# `HISTFILESIZE` is usually set *after* bash reads the history file (which is
# done after reading any configs like `.bashrc`). If it is unset at this point,
# it is set to the same value as `HISTSIZE`. Therefore we must set it to `NIL`,
# in which case it isn't "unset", but doesn't have a value either, enabling us
# to keep an unlimited history.
export HISTFILESIZE=""
