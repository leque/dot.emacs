EMACS_D = $(HOME)/.emacs.d
LISP = $(EMACS_D)/lisp
TO_HOME = .viper .skk
TO_EMACS_D = init.el
TO_LISP = viper-paredit.el gauche-paredit.el skk-pskana.el seikana-ziom.el
INSTALL = install

all:

install:
	$(INSTALL) -c $(TO_HOME) "$(HOME)"
	$(INSTALL) -d "$(EMACS_D)"
	$(INSTALL) -c $(TO_EMACS_D) "$(EMACS_D)"
	$(INSTALL) -d "$(LISP)"
	$(INSTALL) -c $(TO_LISP) "$(LISP)"
