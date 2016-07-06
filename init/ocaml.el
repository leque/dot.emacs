(el-get-bundle caml-mode
  (with-eval-after-load-feature 'caml
    (require 'caml-font))
  )

(defvar opam-share nil)

(defun opam-site-lisp ()
  (and opam-share
       (concat opam-share "/emacs/site-lisp")))

(defun opam-config ()
  (interactive)
  (setq load-path (delete (opam-site-lisp) load-path))
  (when (executable-find "opam")
    (setq opam-share
          (s-chomp
           (shell-command-to-string "opam config var share 2> /dev/null")))
    (add-to-list 'load-path (opam-site-lisp))
    (run-hooks 'opam-config-hook)))

(defun opam-config-merlin ()
  (interactive)
  (when (featurep 'merlin)
    (unload-feature 'merlin t))
  (when (locate-library "merlin")
    (autoload 'merlin-mode "merlin" "Merlin mode" t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    (setq-default merlin-use-auto-complete-mode 'easy)
    (setq-default merlin-command 'opam)))

(add-hook 'opam-config-hook #'opam-config-merlin)

(defun opam-config-ocp-indent ()
  (interactive)
  (when (featurep 'ocp-indent)
    (unload-feature 'ocp-indent t))
  (require 'ocp-indent))

(add-hook 'opam-config-hook #'opam-config-ocp-indent)

(opam-config)
