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

;;;; misc. util
(defun change-frame-width (w)
  (interactive "nnew width: ")
  (set-frame-width (selected-frame) w))

(defun require* (feature &optional filename)
  "loads FEATURE from FILENAME like `require', but never signals error.
returns t if the feature is loaded,
otherwise displays a warnning message and returns nil."
  (let ((res (require feature filename t)))
    (unless res
      (warn "require*: %s is not loaded" (or filename feature)))
    res))

(defun load* (file)
  "loads FILE like `load', but never signals error.
returns t if the file is loaded,
otherwise displays a warnning message and returns nil."
  (let ((res (load file t t)))
    (unless res
      (warn "load*: %s is not loaded" file))
    res))

;;;; Basic Settings
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;;; start server
(require 'server)

(unless (server-running-p)
  (server-start))

(setq inhibit-startup-message t)
(setq initial-frame-alist '())
(setq make-backup-files nil)
(setq require-final-newline t)
(setq default-frame-alist
      `((width . 80)
        (menu-bar-lines . ,(if window-system 1 0))
        (tool-bar-lines . nil)
        (line-spacing . 1)
        ))

(setq-default indent-tabs-mode nil)
(setq-default indicate-buffer-boundaries t)

(blink-cursor-mode t)
(transient-mark-mode nil)

(setq dired-bind-jump nil)

;;; key bindings
(global-set-key (kbd "C-h") #'backward-delete-char)
(global-set-key (kbd "C-x h") #'help)

;;; path
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(el-get-bundle! exec-path-from-shell
  (exec-path-from-shell-initialize)
  )

;;;; Minor-modes
;;; font lock
(require 'font-lock)
(global-font-lock-mode 1)

;;; show tabs, traling spaces, and long lines
(require 'whitespace)
(setq whitespace-style '(face tabs trailing lines-tail))
(global-whitespace-mode)

(custom-set-faces
 '(whitespace-line
   ((t (
        :inherit t
        :background "gray85")))))

;;; auto-complete
(el-get-bundle! auto-complete
  (ac-config-default)
  (setq ac-auto-start nil)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  )

;;; mic-paren
(el-get-bundle! mic-paren
  (paren-activate)
  (setq paren-match-face 'region)
  (setq paren-sexp-mode t))

;;; paredit
(el-get-bundle! paredit
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
  )

;;; Viperize
(setq viper-mode t)
(setq viper-custom-file-name (locate-user-emacs-file ".viper"))
(el-get-bundle leque/viper-lisp)
(require 'viper)

;; kludge to avoid cursor flicker on Cocoa Emacs
(defadvice viper-change-cursor-color
    (around kludge-to-avoid-cursor-flicker-on-Cocoa-Emacs activate)
  nil)

;;; skk
(setq skk-init-file (locate-user-emacs-file ".skk"))

(el-get-bundle ddskk
  (global-set-key (kbd "C-x C-j") #'skk-mode))

(el-get-bundle nicola-ddskk)

;;;; other
(el-get-bundle! emacs-jp/init-loader
  (setq init-loader-show-log-after-init 'error-only)
  (setq init-loader-byte-compile t)
  (setq init-loader-directory (locate-user-emacs-file "init-loader"))
  (init-loader-load))
