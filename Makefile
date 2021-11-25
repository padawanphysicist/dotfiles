SHELL = /bin/bash
DOTFILES_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
XDG_CONFIG_HOME:=$(or $(XDG_CONFIG_HOME),$(HOME)/.config)
STOW=stow --verbose

.PHONY: check \
	bash \
	git \
	environment \
	clean \
	xterm \
	xresources \
	xmonad \
	vim \
	ranger \
	tmux \
	redshift

all: \
	bash \
	git \
	environment \
	xresources \
	xmonad \
	vim \
	ranger \
	tmux \
	redshift \
	xterm

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
	@mkdir --verbose --parents $(XDG_CONFIG_HOME)/git/
	@$(STOW) --target=$(XDG_CONFIG_HOME)/git/ $@

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

vim:
	@$(STOW) --target=$(HOME) $@


redshift:
	@mkdir --verbose --parents $(XDG_CONFIG_HOME)/redshift/
	@$(STOW) --target=$(XDG_CONFIG_HOME)/redshift/ $@

xmonad:
	mkdir -p $(XDG_CONFIG_HOME)/dunst/
	mkdir -p $(XDG_CONFIG_HOME)/picom/
	mkdir -p $(XDG_CONFIG_HOME)/rofi/
	mkdir -p $(HOME)/.xmonad/
	stow --target=$(XDG_CONFIG_HOME)/dunst/ --dir=$(DOTFILES_DIR)/xmonad/ dunst
	stow --target=$(XDG_CONFIG_HOME)/picom/ --dir=$(DOTFILES_DIR)/xmonad/ picom
	stow --target=$(XDG_CONFIG_HOME)/rofi/ --dir=$(DOTFILES_DIR)/xmonad/ rofi
	stow --target=$(HOME) --dir=$(DOTFILES_DIR)/xmonad/ $@

tmux:
	stow --target=$(HOME) $@

ranger:
	mkdir -p $(XDG_CONFIG_HOME)/ranger/
	stow --target=$(HOME) $@

clean:
	@$(STOW) --target=$(HOME) --delete bash
	@$(STOW) --target=$(XDG_CONFIG_HOME)/git/ --delete git
	@$(STOW) --target=$(HOME) --delete environment
	@$(STOW) --target=$(HOME)/.Xresources.d/ --delete xterm
	@$(STOW) --target=$(HOME) --delete xresources
	@$(STOW) --target=$(HOME) --delete vim
	@$(STOW) --target=$(HOME) --delete tmux
	@$(STOW) --target=$(HOME) --delete ranger
	@$(STOW) --target=$(XDG_CONFIG_HOME)/redshift/ --delete redshift
	@$(STOW) --target=$(XDG_CONFIG_HOME)/dunst/ --dir=$(DOTFILES_DIR)/xmonad/ --delete dunst
	@$(STOW) --target=$(XDG_CONFIG_HOME)/picom/ --dir=$(DOTFILES_DIR)/xmonad/ --delete picom
	@$(STOW) --target=$(XDG_CONFIG_HOME)/rofi/ --dir=$(DOTFILES_DIR)/xmonad/ --delete rofi
	@$(STOW) --target=$(HOME) --dir=$(DOTFILES_DIR)/xmonad/ --delete xmonad


