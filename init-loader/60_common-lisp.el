(el-get-bundle slime
  (with-eval-after-load-feature 'slime
    (setq inferior-lisp-program "sbcl")
    (slime-setup '(slime-fuzzy slime-repl))))
