EMACS_D = $(HOME)/.emacs.d
LISP = $(EMACS_D)/lisp
TO_HOME = .viper .skk
TO_EMACS_D = init.el
TO_LISP = viper-paredit.el gauche-paredit.el skk-pskana.el seikana-ziom.el
INSTALL = install
CP = ln -f

all:

install:
	$(CP) $(TO_HOME) "$(HOME)"
	$(INSTALL) -d "$(EMACS_D)"
	$(CP) $(TO_EMACS_D) "$(EMACS_D)"
	$(INSTALL) -d "$(LISP)"
	$(CP) $(TO_LISP) "$(LISP)"
