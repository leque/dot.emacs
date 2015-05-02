EMACS_D = $(HOME)/.emacs.d
LISP = $(EMACS_D)/lisp
TO_HOME = .viper .skk
TO_EMACS_D = init.el
TO_LISP = viper-paredit.el gauche-paredit.el skk-pskana.el seikana-ziom.el
INSTALL = install
CP = ln -sf

all:

install:
	for f in $(TO_HOME); do $(CP) $$PWD/$$f "$(HOME)"; done
	$(INSTALL) -d "$(EMACS_D)"
	for f in $(TO_EMACS_D); do $(CP) $$PWD/$$f "$(EMACS_D)"; done
	$(INSTALL) -d "$(LISP)"
	for f in $(TO_LISP); do $(CP) $$PWD/$$f "$(LISP)"; done
