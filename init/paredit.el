(el-get-bundle! paredit
  (with-eval-after-load-feature 'paredit
    (mapc #'(lambda (mode)
              (add-hook mode #'enable-paredit-mode))
          '(emacs-lisp-mode-hook
            lisp-mode-hook
            lisp-interaction-mode-hook
            scheme-mode-hook
            clojure-mode-hook))
    (defadvice paredit-newline (around eval-print-last-sexp activate)
      (if (eq major-mode 'lisp-interaction-mode)
          (eval-print-last-sexp)
        ad-do-it))
    (define-key paredit-mode-map (kbd "M-]")
      #'paredit-forward-slurp-sexp)
    (define-key paredit-mode-map (kbd "M-[")
      #'paredit-backward-slurp-sexp)
    (define-key paredit-mode-map (kbd "M-}")
      #'paredit-forward-barf-sexp)
    (define-key paredit-mode-map (kbd "M-{")
      #'paredit-backward-barf-sexp)
    ))
