# Configuration for the lf file manager

LFCD="${HOME}/.config/lf/lfcd.sh"
if [ -f "$LFCD" ]; then
    source "$LFCD"
    bind '"\C-o":"lfcd\C-m"'
fi
