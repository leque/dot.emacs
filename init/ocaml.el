(el-get-bundle caml-mode
  (with-eval-after-load-feature 'caml
    (require 'caml-font))
  (autoload 'caml-mode "caml" nil t)
  (push (cons "\\.ml[ily]?\\'" 'caml-mode)
        auto-mode-alist)
  )

(when (executable-find "opam")
  (defvar opam-share
    (substring
     (shell-command-to-string "opam config var share 2> /dev/null")
     0
     -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
  (when (locate-library "merlin")
    (autoload 'merlin-mode "merlin" "Merlin mode" t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    (setq-default merlin-use-auto-complete-mode 'easy)
    (setq-default merlin-command 'opam))
  )
