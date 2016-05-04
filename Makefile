EMACS_D = $(HOME)/.emacs.d
TO_EMACS_D = init.el .skk .viper init init-loader lisp recipes
LOCAL = init-loader/99_local.el
INSTALL = install
CP = ln -sf
GIT = git

.PHONY: help install setup

help: ## show this message
	@awk -F ':.*##' '/^[^	]+:.*##/ { printf "%s\t\t%s\n", $$1, $$2 }' \
	  Makefile \
	| sort

install: ## install files under ~/.emacs.d
	$(INSTALL) -d "$(EMACS_D)"
	for f in $(TO_EMACS_D); do $(CP) $$PWD/$$f "$(EMACS_D)"; done

setup: ## setup files to develop dotfiles
	$(GIT) update-index --skip-worktree $(LOCAL)
