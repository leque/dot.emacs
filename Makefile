EMACS_D = $(HOME)/.emacs.d
TO_EMACS_D = init.el .skk.el .viper.el init-loader lice lisp recipes www
LOCAL = init-loader/99_local.el
INSTALL = install
CP = ln -sf
RM = rm -f
GIT = git
EMACS = emacs -l $(EMACS_D)/init.el
TARGETS = lisp/skk-pskana.el

.PHONY: help install build setup clean test

help: ## show this message
	@awk -F ':.*##' '/^[^	]+:.*##/ { printf "%s\t\t%s\n", $$1, $$2 }' \
	  Makefile \
	| sort

install: build ## install files under ~/.emacs.d
	$(INSTALL) -d "$(EMACS_D)"
	for f in $(TO_EMACS_D); do $(CP) $$PWD/$$f "$(EMACS_D)"; done

build: $(TARGETS)

lisp/skk-pskana.el: lisp/README.skk-pskana.org
	cask emacs --batch \
		-l org-commentary-cli \
		-eval '(setq org-ascii-links-to-notes nil)' \
		-f org-commentary -- $< $@

setup: ## setup files to develop dotfiles
	$(GIT) update-index --skip-worktree $(LOCAL)

clean: ## clean *.elc
	$(GIT) clean -fX
	find . -type l ! -exec test -e {} \; -print | xargs $(RM)

test: install ## run tests for installed files
	$(EMACS) -Q --batch -l test/test-skk-pskana.el -f ert-run-tests-batch-and-exit
