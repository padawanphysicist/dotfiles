TANGLESCRIPT=./org-tangle

help: 
	echo -e "Usage: make <install|update> UPDATE=<0|1>"

install: check_deps tangle

check_deps:
	@command -v stow >/dev/null 2>&1 || { echo -e "\033[0;31mError\033[0m: I require GNU Stow but it's not installed.  Aborting." >&2; exit 1; }
	@command -v emacs >/dev/null 2>&1 || { echo -e "\033[0;31mError\033[0m: I require GNU emacs but it's not installed.  Aborting." >&2; exit 1; }
	@command -v zsh >/dev/null 2>&1 || { echo -e "\033[0;31mError\033[0m: I require Z-shell but it's not installed.  Aborting." >&2; exit 1; }
	@command -v git >/dev/null 2>&1 || { echo -e "\033[0;31mError\033[0m: I require git but it's not installed.  Aborting." >&2; exit 1; }

tangle: zsh tmux git bin festival emacs vim

symlink: tangle
	stow zsh
	stow scripts
	stow tmux
	stow git
	stow festival
	stow emacs
	stow vim

antigen:
	@if [ ! -d "$(HOME)/.antigen" ]; then \
	echo -e "[\033[0;33mW\033[0m] antigen is not installed. Downloading it..."; \
	git clone https://github.com/zsh-users/antigen.git $(HOME)/.antigen; else \
	echo -e "[\033[0;32mM\033[0m] antigen is already installed. Skipping..." ; fi

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

spf13:
	@if [ ! -d "${HOME}/.spf13-vim-3" ]; then \
	echo "Bootstraping $@ configuration..." \
	echo -e "[\033[0;33mW\033[0m] spf13-vim is not installed. Downloading it..."; \
	curl http://j.mp/spf13-vim3 -L -o - | sh ; else \
	echo -e "[\033[0;32mM\033[0m] spf13-vim is already installed. Skipping..." ; fi

vim: spf13
	@if [ ! -d "./$@" ]; then mkdir $@; fi
	@$(TANGLESCRIPT) $@.org

spacemacs:
	@if [ ! -d "${HOME}/.emacs.d" ]; then \
	echo -e "[\033[0;33mW\033[0m] spacemacs is not installed. Downloading it..."; \
	git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d; \
	elif [ ! -d "${HOME}/.emacs.d/.git" ]; then \
	echo  -e "[\033[0;33mW\033[0m] spacemacs is not installed. Downloading it..."; \
	git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d; else \
	echo  -e "[\033[0;32mM\033[0m] spacemacs is already installed. Skipping..."; fi

zsh: dircolors antigen
	@echo "Bootstraping $@ configuration...";
	@if [ ! -d "./$@" ]; then mkdir $@; fi
	@$(TANGLESCRIPT) $@.org
bin:
	@echo "Bootstraping $@ configuration..."
	@if [ ! -d "./scripts/.$@" ]; then mkdir -p scripts/.$@; fi
	@$(TANGLESCRIPT) $@.org
	@chmod +x scripts/.$@/*
git:
	@echo "Bootstraping $@ configuration..."
	@if [ ! -d "./$@" ]; then mkdir $@; fi
	@$(TANGLESCRIPT) $@.org
festival:
	@echo "Bootstraping $@ configuration..."
	@if [ ! -d "./$@" ]; then mkdir $@; fi
	@$(TANGLESCRIPT) $@.org
tmux:
	@echo "Bootstraping $@ configuration..."
	@if [ ! -d "./$@" ]; then mkdir $@; fi
	@$(TANGLESCRIPT) $@.org
emacs: spacemacs
	@echo "Bootstraping $@ configuration..."
	@if [ ! -d "./emacs" ]; then mkdir -p emacs/.elisp; fi
	@$(TANGLESCRIPT) emacs.org
clean:
	rm -fr zsh emacs vim tmux scripts festival git
	rm -fr ~/.dircolors-solarized ~/.antigen ~/.vim ~/.spf13-vim-3
