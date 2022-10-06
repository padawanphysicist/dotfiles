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
	xmonad \
	ranger \
	tmux \
	redshift \
	rofi \
	polybar \
	picom \
	stumpwm \
	dunst \
	lf

all: \
	bash \
	git \
	xresources \
	xmonad \
	ranger \
	tmux \
	redshift \
	xterm \
	rofi \
	polybar \
	picom \
	stumpwm \
	dunst \
	lf

check:
	@echo "Checking dependencies:"
	@for d in 'stow'; do	\
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

lf:
	@mkdir --verbose --parents $(XDG_CONFIG_HOME)/lf/
	@$(STOW) --target=$(XDG_CONFIG_HOME)/lf/ $@

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

stumpwm:
	@mkdir --verbose --parents $(HOME)/.stumpwm.d/
	@$(STOW) --target=$(HOME)/.stumpwm.d/ $@

polybar:
	@mkdir --verbose --parents $(XDG_CONFIG_HOME)/polybar/
	@$(STOW) --target=$(XDG_CONFIG_HOME)/polybar/ $@

picom:
	@mkdir --verbose --parents $(XDG_CONFIG_HOME)/picom/
	@$(STOW) --target=$(XDG_CONFIG_HOME)/picom/ $@

dunst:
	@mkdir --verbose --parents $(XDG_CONFIG_HOME)/dunst/
	stow --target=$(XDG_CONFIG_HOME)/dunst/ $@

xmonad: rofi polybar picom dunst
	mkdir -p $(HOME)/.xmonad/
	stow --target=$(HOME) --dir=$(DOTFILES_DIR)/xmonad/ $@

tmux:
	stow --target=$(HOME) $@

ranger:
	mkdir -p $(XDG_CONFIG_HOME)/ranger/
	stow --target=$(HOME) $@

clean:
	@$(STOW) --target=$(HOME) --delete bash
	@$(STOW) --target=$(HOME) --delete git
	@$(STOW) --target=$(XDG_CONFIG_HOME)/lf/ --delete lf
	@$(STOW) --target=$(HOME)/.Xresources.d/ --delete xterm
	@$(STOW) --target=$(HOME) --delete xresources
	@$(STOW) --target=$(HOME) --delete tmux
	@$(STOW) --target=$(HOME) --delete ranger
	@$(STOW) --target=$(XDG_CONFIG_HOME)/redshift/ --delete redshift
	@$(STOW) --target=$(XDG_CONFIG_HOME)/picom/ --delete picom
	@$(STOW) --target=$(XDG_CONFIG_HOME)/rofi/ --delete rofi
	@$(STOW) --target=$(XDG_CONFIG_HOME)/dunst/ --delete dunst
	@$(STOW) --target=$(XDG_CONFIG_HOME)/polybar/ --delete polybar
	@$(STOW) --target=$(HOME) --dir=$(DOTFILES_DIR)/xmonad/ --delete xmonad
	@$(STOW) --target=$(HOME)/stumpwm.d/ --delete stumpwm


