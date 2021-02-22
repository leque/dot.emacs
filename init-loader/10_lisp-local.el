(el-get-bundle elpa:lisp-local
  (cl-loop for mode-hook in '(
                              emacs-lisp-mode-hook
                              lisp-mode-hook
                              scheme-mode-hook
                              clojure-mode-hook
                              )
           do (add-hook mode-hook 'lisp-local)))
