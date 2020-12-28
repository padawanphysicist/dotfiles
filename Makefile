CONFIG = dunst emacs git misc rofi shell tmux xmonad xresources bspwm redshift ranger picom doom R
all: $(CONFIG)

# A phony target is one that is not really the name of a file; rather it is
# just a name for a recipe to be executed when you make an explicit request.
.PHONY: clean force misc

bspwm: bspwm.org
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
redshift: redshift.org
ranger: ranger.org
picom: picom.org
doom: doom.org
R: R.org

%: %.org
	if [ -d "$@" ]; then stow --delete $@; rm -rf $@ ; fi
	./tangle -f $<
	stow --stow $@
force:
	touch *.org
clean: 
	stow --delete $(CONFIG)
	for i in $(CONFIG); do if [[ $$i != "misc" ]]; then rm -rf $$i; fi; done
