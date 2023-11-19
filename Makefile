SHELL = /bin/bash
DOTFILES_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
XDG_CONFIG_HOME:=$(or $(XDG_CONFIG_HOME),$(HOME)/.config)
STOW=stow --verbose

.PHONY: check \
	bash \
	git \
	clean \
	xterm \
	xresources \
	ranger \
	tmux \
	redshift \
	rofi \
	picom \
	dunst

all: \
	bash \
	git \
	xresources \
	tmux \
	redshift \
	xterm \
	rofi \
	picom \
	dunst \
	ranger

check:
	@echo "Checking dependencies:"
	@for d in 'stow' 'dwm' 'st' 'rofi' 'picom' 'dunst' 'ranger'; do	\
		if ! command -v "$$d" &> /dev/null; then	\
			echo -e "    - [MISSING] $$d could not be found";	\
		else	\
			echo -e "    - [Installed] $$d"; 			\
		fi;	\
	done;
bash:
	@$(STOW) --target=$(HOME) $@

git:
	@$(STOW) --target=$(HOME) $@

environment:
	@mkdir --verbose --parents $(HOME)/.local/bin/
	@$(STOW) --target=$(HOME) $@

xterm:
	@mkdir --verbose --parents $(HOME)/.Xresources.d/
	@$(STOW) --target=$(HOME)/.Xresources.d/ $@

xresources:
	@mkdir --verbose --parents $(HOME)/.Xresources.d/
	@mkdir --verbose --parents $(HOME)/.local/bin/
	@$(STOW) --target=$(HOME) $@

redshift:
	@mkdir --verbose --parents $(XDG_CONFIG_HOME)/redshift/
	@$(STOW) --target=$(XDG_CONFIG_HOME)/redshift/ $@

rofi:
	@mkdir --verbose --parents $(XDG_CONFIG_HOME)/rofi/
	@$(STOW) --target=$(XDG_CONFIG_HOME)/rofi/ $@

picom:
	@mkdir --verbose --parents $(XDG_CONFIG_HOME)/picom/
	@$(STOW) --target=$(XDG_CONFIG_HOME)/picom/ $@

dunst:
	@mkdir --verbose --parents $(XDG_CONFIG_HOME)/dunst/
	stow --target=$(XDG_CONFIG_HOME)/dunst/ $@

tmux:
	stow --target=$(HOME) $@

ranger:
	mkdir -p $(XDG_CONFIG_HOME)/ranger/
	stow --target=$(HOME) $@

clean:
	@$(STOW) --target=$(HOME) --delete bash
	@$(STOW) --target=$(HOME) --delete git
	@$(STOW) --target=$(HOME)/.Xresources.d/ --delete xterm
	@$(STOW) --target=$(HOME) --delete xresources
	@$(STOW) --target=$(HOME) --delete tmux
	@$(STOW) --target=$(HOME) --delete ranger
	@$(STOW) --target=$(XDG_CONFIG_HOME)/redshift/ --delete redshift
	@$(STOW) --target=$(XDG_CONFIG_HOME)/picom/ --delete picom
	@$(STOW) --target=$(XDG_CONFIG_HOME)/rofi/ --delete rofi
	@$(STOW) --target=$(XDG_CONFIG_HOME)/dunst/ --delete dunst


