CONFIG = dunst emacs git misc rofi shell tmux xmonad xresources 
all: $(CONFIG)

# A phony target is one that is not really the name of a file; rather it is
# just a name for a recipe to be executed when you make an explicit request.
.PHONY: clean force misc

dunst: dunst.org
emacs: emacs.org
git: git.org
misc: misc/
	stow --restow $@
rofi: rofi.org
shell: shell.org
tmux: tmux.org
xmonad: xmonad.org
xresources: xresources.org

%: %.org
	if [ -d "$@" ]; then stow --delete $@; rm -rf $@ ; fi
	./tangle -f $<
	stow --stow $@
force:
	touch *.org
clean: 
	stow --delete $(CONFIG)
	
