;; -*- coding: utf-8-unix -*-
(when (require 'package nil t)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))
  (package-initialize))

(require 'cl)

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

(defun add-to-load-path&recompile (dir)
  (add-to-list 'load-path dir)
  (byte-recompile-directory dir 0))

(defun require* (feature &optional filename)
  "loads feature like require, but never signals error.
returns t if feature is loaded, otherwise nil.
Failed to load warns error message."
  (let ((res (require feature filename t)))
    (unless res
      (warn "require*: %s is not loaded" (or filename feature)))
    res))

;;; path
(add-to-load-path&recompile (expand-file-name "~/.emacs.d/lisp"))

(when (require* 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;;; environment-dependent setting
(set-language-environment 'Japanese)

(cond
 ;; Cocoa
 ((eq window-system 'ns)
  (setq ns-command-modifier 'meta)
  ;; super is used in ns-win.el
  (setq ns-option-modifier 'hyper)
  (set-default-coding-systems 'utf-8)

  (let* ((name "hiragino")
         (fs-name (format "fontset-%s" name))
         (monaco "Monaco-14:weight=normal:slant=normal")
         (hiragino '("Hiragino Maru Gothic Pro" . "iso10646-1")))
    (create-fontset-from-ascii-font monaco nil name)
    (mapc #'(lambda (x)
              (set-fontset-font fs-name x hiragino))
          '(japanese-jisx0208
            japanese-jisx0212
            katakana-jisx0201
            jisx0201
            ))
    (setq face-font-rescale-alist
          '(("^-apple-hiragino.*" . 1.2)))
    (set-face-attribute 'default t :font fs-name)
    (push `(font . ,fs-name) default-frame-alist))
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

;;; Viperize
(setq viper-mode t)
(require 'viper)

;; kludge to avoid cursor flicker on Cocoa Emacs
(defadvice viper-change-cursor-color
  (around kludge-to-avoid-cursor-flicker-on-Cocoa-Emacs activate)
  nil)

;;; completion-ui
(when (require* 'completion-ui)
  (setq completion-auto-show nil)
  (define-key completion-overlay-map "\C-n" #'completion-cycle)
  (define-key completion-overlay-map "\C-p" #'completion-cycle-backwards)
  (define-key completion-overlay-map "\C-j" #'completion-accept)
  (define-key completion-overlay-map "\C-g" #'completion-reject))

;;; mic-paren
(when (require* 'mic-paren)
  (paren-activate)
  (setq paren-match-face 'region)
  (setq paren-sexp-mode t))

;;; paredit
(when (require* 'paredit)
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

;;; skk
(require* 'skk-setup)

;;; qs-complete
(defun friend-major-mode-p (other-buffer)
  (let ((mod (save-excursion
                 (set-buffer other-buffer)
                 major-mode)))
    (or (eq major-mode mod)
        (and (memq mod
                   (find major-mode
                         friend-major-modes
                         :test #'memq))
             t))))

(defvar friend-major-modes
  '((emacs-lisp-mode lisp-interaction-mode)
    ))

(when (and (require* 'completion-ui)
           (require* 'qs-complete))
  (global-set-key [(meta ??)]
                  #'(lambda ()
                      (interactive)
                      (let ((dabbrev-check-all-buffers nil)
                            (dabbrev-friend-buffer-function
                             #'friend-major-mode-p))
                        (complete-qs)))))

;;; zsh-like minibuffer completion
(require* 'zlc)

;;; haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;; ruby mode
(setq ruby-indent-level 3)

;;; scala-mode
(when (require* 'scala-mode-auto)
  (defadvice scala-block-indentation (around improve-indentation-after-brace activate)
    (if (eq (char-before) ?\{)
        (setq ad-return-value (+ (current-indentation) scala-mode-indent:step))
      ad-do-it)))

;;; SLIME
(setq inferior-lisp-program "sbcl")
(when (require* 'slime)
  (slime-setup '(slime-fuzzy slime-repl)))

;;; Gauche Mode
(push '("gosh" . (utf-8 . utf-8)) process-coding-system-alist)

(mapcar #'(lambda (ext)
            (push (cons ext 'gauche-mode) auto-mode-alist))
        '("\\.scm\\'"))

(push '("gauche-refj\\.info.*" utf-8 . utf-8)
      file-coding-system-alist)

(require 'gauche-mode)
(when (featurep 'paredit)
  (require 'gauche-paredit))

(eval-after-load "cmuscheme"
  '(progn
     (defadvice scheme-send-region (after show-ischeme-buffer activate)
       "show *scheme* buffer always"
       (let ((buf (and scheme-buffer
                       (get-buffer scheme-buffer))))
         (when (loop for frame in (frame-list)
                     never (loop for w in (window-list frame)
                                 thereis (eq buf (window-buffer w))))
           (switch-to-buffer-other-window buf)
           (goto-char (point-max))
           (other-window 1))))
     (mapc '(lambda (s)
              (put (car s) 'scheme-indent-function (cdr s)))
           '((define/cmdargs . 1)
             (let-cmdargs . 2)
             (parse-cmdargs . 1)
             ))
     ))

;;; c-mode
(eval-after-load "cc-mode"
  '(progn
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
     ))

(when (require* 'caml)
  (require 'caml-font)
  (push (cons "\\.ml[ily]?\\'" 'caml-mode)
        auto-mode-alist))
