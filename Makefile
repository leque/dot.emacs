EMACS_D = $(HOME)/.emacs.d
TO_HOME = .viper .skk
TO_EMACS_D = init.el init init-loader lisp recipes
INSTALL = install
CP = ln -sf

all:

install:
	for f in $(TO_HOME); do $(CP) $$PWD/$$f "$(HOME)"; done
	$(INSTALL) -d "$(EMACS_D)"
	for f in $(TO_EMACS_D); do $(CP) $$PWD/$$f "$(EMACS_D)"; done
