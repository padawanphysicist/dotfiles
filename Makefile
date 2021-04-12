SHELL = /bin/bash
DOTFILES_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
XDG_CONFIG_HOME:=$(or $(XDG_CONFIG_HOME),$(HOME)/.config)

.PHONY: shell clean terminal xresources-themes scripts git wm-xmonad nvim ranger programming tmux

all: terminal shell xresources-themes scripts git wm-xmonad nvim ranger programming tmux

wm-xmonad:
	mkdir -p $(XDG_CONFIG_HOME)/dunst/
	mkdir -p $(XDG_CONFIG_HOME)/picom/
	mkdir -p $(XDG_CONFIG_HOME)/redshift/
	mkdir -p $(XDG_CONFIG_HOME)/rofi/
	mkdir -p $(HOME)/.xmonad/
	stow --target=$(XDG_CONFIG_HOME)/dunst/ --dir=$(DOTFILES_DIR)/wm-xmonad/ dunst
	stow --target=$(XDG_CONFIG_HOME)/picom/ --dir=$(DOTFILES_DIR)/wm-xmonad/ picom
	stow --target=$(XDG_CONFIG_HOME)/redshift/ --dir=$(DOTFILES_DIR)/wm-xmonad/ redshift
	stow --target=$(XDG_CONFIG_HOME)/rofi/ --dir=$(DOTFILES_DIR)/wm-xmonad/ rofi
	stow --target=$(HOME) --dir=$(DOTFILES_DIR)/wm-xmonad/ xmonad
nvim:
	mkdir -p $(XDG_CONFIG_HOME)/nvim/
	stow --target=$(XDG_CONFIG_HOME)/nvim/ nvim
git:
	mkdir -p $(XDG_CONFIG_HOME)/git/
	stow --target=$(XDG_CONFIG_HOME)/git/ git
scripts:
	mkdir -p $(HOME)/.local/bin/
	stow --target=$(HOME)/.local/bin/ scripts
terminal:
	mkdir -p $(HOME)/.Xresources.d/
	stow --target=$(HOME)/.Xresources.d/ terminal
shell:
	stow --target=$(HOME) shell
tmux:
	stow --target=$(HOME) tmux
programming:
	stow --target=$(HOME) programming
ranger:
	mkdir -p $(XDG_CONFIG_HOME)/ranger/
	stow --target=$(HOME) ranger
xresources-themes:
	mkdir -p $(HOME)/.Xresources.d/
	stow --target=$(HOME)/.Xresources.d/ xresources-themes
clean:
	stow --target=$(HOME) --delete shell
	stow --target=$(HOME) --delete programming
	stow --target=$(HOME) --delete tmux
	stow --target=$(HOME)/.Xresources.d/ --delete terminal
	stow --target=$(HOME)/.Xresources.d/ --delete xresources-themes
	stow --target=$(XDG_CONFIG_HOME)/git/ --delete git
	stow --target=$(XDG_CONFIG_HOME)/nvim/ --delete nvim
	stow --target=$(XDG_CONFIG_HOME)/dunst/ --dir=$(DOTFILES_DIR)/wm-xmonad/ --delete dunst
	stow --target=$(XDG_CONFIG_HOME)/picom/ --dir=$(DOTFILES_DIR)/wm-xmonad/ --delete picom
	stow --target=$(XDG_CONFIG_HOME)/redshift/ --dir=$(DOTFILES_DIR)/wm-xmonad/ --delete redshift
	stow --target=$(XDG_CONFIG_HOME)/rofi/ --dir=$(DOTFILES_DIR)/wm-xmonad/ --delete rofi
	stow --target=$(HOME) --dir=$(DOTFILES_DIR)/wm-xmonad/ --delete xmonad
	stow --target=$(HOME) --delete ranger
	stow --target=$(HOME)/.local/bin/ --delete scripts
