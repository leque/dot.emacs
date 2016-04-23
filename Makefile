EMACS_D = $(HOME)/.emacs.d
TO_EMACS_D = init.el .skk .viper init init-loader lisp recipes
LOCAL = init-loader/99_local.el
INSTALL = install
CP = ln -sf
GIT = git

.PHONY: all install setup

all:

install:
	$(INSTALL) -d "$(EMACS_D)"
	for f in $(TO_EMACS_D); do $(CP) $$PWD/$$f "$(EMACS_D)"; done

setup:
	$(GIT) update-index --skip-worktree $(LOCAL)
