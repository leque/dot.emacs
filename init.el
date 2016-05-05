;; -*- coding: utf-8-unix -*-
(require 'cl-lib)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;;; el-get
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (locate-user-emacs-file "recipes"))

(el-get-bundle tarao/with-eval-after-load-feature-el)

;;; start server
(require 'server)

(unless (server-running-p)
  (server-start))

;;; path
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(el-get-bundle! exec-path-from-shell
  (exec-path-from-shell-initialize)
  )

;;; Viperize
(setq viper-mode t)
(setq viper-custom-file-name (locate-user-emacs-file ".viper"))
(el-get-bundle leque/viper-lisp)
(require 'viper)

;;;; other
(el-get-bundle! emacs-jp/init-loader
  (setq init-loader-show-log-after-init 'error-only)
  (setq init-loader-byte-compile t)
  (setq init-loader-directory (locate-user-emacs-file "init-loader"))
  (init-loader-load))
