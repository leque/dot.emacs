;; -*- coding: utf-8-unix -*-

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; (package-initialize)
(setq gc-cons-threshold (* 256 1024 1024)) ;; 256 MiB
(setq garbage-collection-messages t)

(desktop-save-mode +1)

(require 'cl-lib)

(setq default-directory (expand-file-name "~/"))

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(setq custom-file (locate-user-emacs-file "custom.el"))

(let ((auto-save-dir (locate-user-emacs-file "auto-save/")))
  (make-directory auto-save-dir t)
  (setq auto-save-file-name-transforms (list
                                        (list (rx (* any)) auto-save-dir t))))
(setq auto-save-timeout 10)
(setq auto-save-interval 100)

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

(setq load-prefer-newer t)

(el-get-bundle! auto-compile
  (auto-compile-on-load-mode))

;;; start server
(require 'server)

(unless (server-running-p)
  (server-start))

;;; path
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(unless (featurep 'dos-w32)
  (el-get-bundle! exec-path-from-shell
    (exec-path-from-shell-initialize)
    ))

;;; Viperize
(setq viper-mode t)
(setq viper-custom-file-name (locate-user-emacs-file ".viper.el"))
(el-get-bundle leque/viper-lisp)
(require 'viper)

;;; persistent-scratch
(el-get-bundle! persistent-scratch
  (persistent-scratch-setup-default))

;;;; other
(el-get-bundle! emacs-jp/init-loader
  (setq init-loader-show-log-after-init 'error-only)
  (setq init-loader-byte-compile t)
  (setq init-loader-directory (locate-user-emacs-file "init-loader"))
  (init-loader-load))

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
