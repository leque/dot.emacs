;; -*- coding: utf-8-unix -*-
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (locate-user-emacs-file "recipes"))

(el-get-bundle tarao/with-eval-after-load-feature-el)

(require 'cl-lib)

;;; start server
(require 'server)

(unless (server-running-p)
  (server-start))

;;;; Basic Settings
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

;;; key bindings
(global-set-key [(control ?h)] #'backward-delete-char)
(global-set-key [(control ?x) ?h] #'help)

;;; misc. util
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

;;; path
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(el-get-bundle! exec-path-from-shell
  (exec-path-from-shell-initialize)
  )

;;; environment-dependent setting
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

(cond
 ;; Cocoa
 ((eq window-system 'ns)
  (setq ns-command-modifier 'meta)
  ;; super is used in ns-win.el
  (setq ns-option-modifier 'hyper)

  (let ((size 14)
        (ascii-font "Monaco")
        (ja-font "Hiragino Maru Gothic Pro"))
    (set-face-attribute 'default nil
                        :family ascii-font
                        :height (* size 10))
    (mapc #'(lambda (x)
              (set-fontset-font t x ja-font))
          '(katakana-jisx0201
            japanese-jisx0208
            japanese-jisx0212
            japanese-jisx0213-1
            japanese-jisx0213-2
            ))
    (set-fontset-font t '(#x0080 . #x024F) ascii-font)
    (setq frame-inherited-parameters '(font tool-bar-lines))
    (add-to-list 'face-font-rescale-alist
                 '(".*Hiragino.*" . 1.2)))
  )
 ;; Carbon
 ((eq window-system 'mac)
  (require 'carbon-font)

  (fixed-width-set-fontset "hiramaru" 14)

  (setq mac-allow-anti-aliasing t)
  (setq mac-ts-script-language-on-focus '(6 . 6))

  (setq file-name-coding-system 'utf-8)
  (set-clipboard-coding-system  'sjis-mac)
  (set-keyboard-coding-system   'sjis-mac)
  )
 ((eq window-system 'w32)
  (setq file-name-coding-system 'sjis-dos)
  (set-buffer-file-coding-system        'sjis-dos)
  (set-clipboard-coding-system  'sjis-dos)
  (set-default-coding-systems   'sjis-dos)
  (set-keyboard-coding-system   'sjis-dos)
  (set-terminal-coding-system   'sjis-dos)
  (set-w32-system-coding-system 'sjis-dos)
  )
 )

;;;; Minor-modes
;;; font lock
(require 'font-lock)
(global-font-lock-mode 1)

;;; show tabs and traling spaces
(require 'whitespace)
(setq whitespace-style '(face tabs))
(setq-default show-trailing-whitespace t)
(global-whitespace-mode)

;;; auto-complete
(el-get-bundle! auto-complete
  (ac-config-default)
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
  (define-key paredit-mode-map [(meta ?\])]
    #'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map [(meta ?\[)]
    #'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map [(meta ?\})]
    #'paredit-forward-barf-sexp)
  (define-key paredit-mode-map [(meta ?\{)]
    #'paredit-backward-barf-sexp)
  )

;;; Viperize
(setq viper-mode t)
(el-get-bundle leque/viper-lisp)
(require 'viper)

;; kludge to avoid cursor flicker on Cocoa Emacs
(defadvice viper-change-cursor-color
  (around kludge-to-avoid-cursor-flicker-on-Cocoa-Emacs activate)
  nil)

;;; skk
(el-get-bundle ddskk
  (global-set-key (kbd "C-x C-j") #'skk-mode))

;;; haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;; ruby mode
(setq ruby-indent-level 3)

;;; scala-mode
(el-get-bundle scala-mode2)

;;; SLIME
(setq inferior-lisp-program "sbcl")
(el-get-bundle slime
  (with-eval-after-load-feature 'slime
    (slime-setup '(slime-fuzzy slime-repl))))

;;; Gauche Mode
(setq gauche-mode-info-language 'ja)
(el-get-bundle leque/gauche-mode
  (push '("gosh" . (utf-8 . utf-8)) process-coding-system-alist)
  (push '("gauche-refj\\.info.*" utf-8 . utf-8)
        file-coding-system-alist)
  (with-eval-after-load 'gauche-mode
    (require 'gauche-paredit)
    (require 'ac-gauche))
  )

(with-eval-after-load "cmuscheme"
  (defadvice scheme-send-region (after show-ischeme-buffer activate)
    "show *scheme* buffer always"
    (let ((buf (and scheme-buffer
                    (get-buffer scheme-buffer))))
      (when (cl-loop for frame in (frame-list)
                     never (cl-loop for w in (window-list frame)
                                    thereis (eq buf (window-buffer w))))
        (switch-to-buffer-other-window buf)
        (goto-char (point-max))
        (other-window 1))))
  )

;;; c-mode
(with-eval-after-load "cc-mode"
  (c-add-style "ruby"
               '("bsd"
                 (c-basic-offset . 4)
                 (c-offsets-alist
                  (case-label . 2)
                  (label . 2)
                  (statement-case-intro . 2)
                  (statement-case-open . 2))))

  (c-add-style "gauche"
               '("bsd"
                 (c-basic-offset . 4)
                 (c-offsets-alist
                  (label . 2)
                  (statement-case-open . 4))))
  )

(el-get-bundle caml-mode
  (with-eval-after-load-feature 'caml
    (require 'caml-font))
  (autoload 'caml-mode "caml" nil t)
  (push (cons "\\.ml[ily]?\\'" 'caml-mode)
        auto-mode-alist)
  )

;; Add opam emacs directory to the load-path
(when (locate-file "opam" exec-path)
  (setq opam-share
        (substring
         (shell-command-to-string "opam config var share 2> /dev/null")
         0
         -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp")))

(progn
  (autoload 'merlin-mode "merlin" "Merlin mode" t)
  (add-hook 'caml-mode-hook 'merlin-mode t)
  (setq merlin-use-auto-complete-mode 'easy)
  (setq merlin-command 'opam))

(el-get-bundle sml-mode
  (with-eval-after-load-feature 'sml-mode
    (setq sml-indent-level 2)
    (push '("\\.\\(sml\\|smi\\|sig\\)\\'" . sml-mode)
          auto-mode-alist)))

(el-get-bundle "ProofGeneral")
