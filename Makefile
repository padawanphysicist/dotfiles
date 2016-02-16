BOOTSTRAP=./bootstrap

help:
	echo -e "Usage: make <install|update> UPDATE=<0|1>"

install: zsh git festival tmux vim emacs scripts

check_deps:
	@command -v stow >/dev/null 2>&1 || { echo -e "\033[0;31mError\033[0m: I require GNU Stow but it's not installed.  Aborting." >&2; exit 1; }
	@command -v emacs >/dev/null 2>&1 || { echo -e "\033[0;31mError\033[0m: I require GNU emacs but it's not installed.  Aborting." >&2; exit 1; }
	@command -v zsh >/dev/null 2>&1 || { echo -e "\033[0;31mError\033[0m: I require Z-shell but it's not installed.  Aborting." >&2; exit 1; }
	@command -v git >/dev/null 2>&1 || { echo -e "\033[0;31mError\033[0m: I require git but it's not installed.  Aborting." >&2; exit 1; }


update:
# Update zsh antigen
	@echo -e "[\033[0;33mW\033[0m] Updating antigen..."
	@cd $(HOME)/.antigen && git pull
# Update dircolors
	@echo -e "[\033[0;33mW\033[0m] Updating dircolors-solarized..."
	@cd $(HOME)/.dircolors-solarized && git pull
# Update spf13-vim3
	@echo -e "[\033[0;33mW\033[0m] Updating spf13-vim3..."
	@cd $(HOME)/.spf13-vim-3 && git pull
# Update spacemacs
	@cd $(HOME)/.emacs.d/ && git fetch && git reset --hard origin/master
#&& vim +BundleInstall! +BundleClean +q
# Show some instructions to the user
	@echo -e "[\033[0;32mM\033[0m] Don't forget to update the plugins! For this, run \033[0;34mantigen update\033[0m"

dircolors:
	@if [ ! -d "$(HOME)/.dircolors-solarized" ]; then \
	echo -e "[\033[0;33mW\033[0m] dircolors-solarized is not installed. Downloading it..."; \
	git clone https://github.com/seebi/dircolors-solarized $(HOME)/.dircolors-solarized; else \
	echo -e "[\033[0;32mM\033[0m] dircolors-solarized is already installed. Skipping..." ; fi
antigen:
	@if [ ! -d "$(HOME)/.antigen" ]; then \
	echo -e "[\033[0;33mW\033[0m] antigen is not installed. Downloading it..."; \
	git clone https://github.com/zsh-users/antigen.git $(HOME)/.antigen; else \
	echo -e "[\033[0;32mM\033[0m] antigen is already installed. Skipping..." ; fi

vundle:
	@if [ ! -d "${HOME}/.vim" ] || [ ! -d "${HOME}/.vim/bundle" ]; then \
	echo -e "\r  [\033[0;33mW\033[0m] Vundle is not installed. Downloading it..."; \
	mkdir -p ~/.vim/bundle; \
	git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim; else \
	if [ -d "${HOME}/.vim/bundle" ] ; then \
	echo -e "\r  [\033[0;32mM\033[0m] Vundle is already installed. Skipping..." ; fi; fi

vim: vundle
	@./bootstrap -i $@ -f
	vim +PluginInstall +qall

zsh: dircolors antigen
	@./bootstrap -i $@ -f
scripts:
	@echo "Bootstraping $@ configuration..."
	@./bootstrap -i $@ -f
	@chmod +x $@/*
git:
	@echo "Bootstraping $@ configuration..."
	@./bootstrap -i $@ -f
festival:
	@echo "Bootstraping $@ configuration..."
	@./bootstrap -i $@ -f
tmux:
	@echo "Bootstraping $@ configuration..."
	@./bootstrap -i $@ -f

spacemacs:
	@if [ ! -d "${HOME}/.emacs.d" ] || [ ! -d "${HOME}/.emacs.d/.git" ]; then \
	echo -e "\r  [\033[0;33mW\033[0m] spacemacs is not installed. Downloading it..."; \
	git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d; else \
	if [ -d "${HOME}/.emacs.d/.git" ] ; then \
	echo -e "\r  [\033[0;32mM\033[0m] spacemacs is already installed. Skipping..." ; fi; fi

emacs: spacemacs
	@./bootstrap -i $@ -f
	@echo -e "\r  [\033[0;33mW\033[0m] Starting emacs so it can finish package installation. Close it when done.";
	@emacs

cleanall:
	rm -fr zsh emacs vim tmux scripts festival git
	rm -f ~/.zshrc ~/.spacemacs ~/.nvimrc ~/.vimrc ~/.tmux.conf ~/.bin ~/.festivalrc ~/.gitconfig
	rm -fr ~/.dircolors-solarized ~/.antigen ~/.emacs.d ~/.elisp ~/.nvim ~/.vim
