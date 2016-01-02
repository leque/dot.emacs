EMACS_D = $(HOME)/.emacs.d
TO_EMACS_D = init.el .skk .viper init init-loader lisp recipes
INSTALL = install
CP = ln -sf

all:

install:
	$(INSTALL) -d "$(EMACS_D)"
	for f in $(TO_EMACS_D); do $(CP) $$PWD/$$f "$(EMACS_D)"; done
