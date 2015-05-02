(setq viper-inhibit-startup-message t)
(setq viper-expert-level 5)

(setq ex-cycle-other-window nil)
(setq viper-allow-multiline-replace-regions nil)

;;; set autoindent shiftwidth=4
(setq viper-shift-width 4)
(setq-default viper-auto-indent t)

;;; some modes do not enable viper-mode correctly.
(setq viper-vi-state-mode-list
      (append '(
                clojure-mode
                scala-mode
                tuareg-mode
                fuel-mode
                scheme-mode
                haskell-mode
                )
              viper-vi-state-mode-list))


;;;; Key bindings

(cond
 ((boundp 'input-decode-map)
  (define-key input-decode-map [?\e] [escape]))
 (t
  (define-key viper-insert-global-user-map [(control ?\[)]
    #'viper-intercept-ESC-key)

  (define-key viper-vi-global-user-map [(control ?\[)]
    #'undefined)))


;;; Keybindings for Command mode
;;;
;;; g, K, q, V, v, ^A, ^C, ^K, ^O, ^Q, ^S, ^T, ^V, ^W, ^X, ^\, ^_, *
;;; and \ are free in traditional vi.
;;;
;;; NB: ^C is used for interrupt.  ^Q and ^S are used for flow
;;; control.  ^_ is said to be reserved as the command character for
;;; the Tektronix 4025 and 4027 terminal(?).
;;;
;;; cf. Keys used in nvi:
;;;   * ^A: search forward for the current word
;;;   * ^T: pop tag
;;;   * ^W: switch to next lower screen
;;;   * ^]: push tag
;;;
;;; g, K, q, _ are free in viper too.
;;; * Keys used by viper:
;;;   * V: find-file-in-other-window
;;;   * v: find-file
;;;   * *: call-last-kbd-macro
;;;   * \: viper-escape-to-emacs
;;;   * ^V: find-file-other-frame
;;; * Keys used by Emacs:
;;;   * ^A: move-beginning-of-line
;;;   * ^K: kill-line
;;;   * ^O: open-line
;;;   * ^Q: quoted-insert
;;;   * ^T: transpose-chars
;;;   * ^W: kill-region
;;;   * ^X: <command prefix>
;;;   * ^]: abort-recursive-edit
;;;   * ^_: undo
;;; * Keys bounded to different command from traditional vi
;;;   * Q: viper-query-replace (`:ex' in vi)
;;;   * #: viper-special-prefix-com (increment/decrement in nvi)

(defvar viper-vi-my-g-map (make-sparse-keymap))

(define-key viper-vi-global-user-map "g" viper-vi-my-g-map)

(define-key viper-vi-my-g-map "g"
  #'(lambda ()
      (interactive)
      (viper-goto-line 1)))

(defun splice-sexp ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (save-excursion
      (forward-sexp)
      (backward-delete-char 1))
    (delete-char 1)
    (backward-up-list)
    (indent-sexp)))

(define-key viper-vi-global-user-map "K" #'raise-sexp)
(define-key viper-vi-global-user-map "v" #'splice-sexp)

(when (require 'viper-lisp nil t)
  ;; automatically `:set lisp' in lisp-related modes.
  (mapc #'(lambda (mode)
            (add-hook mode
                      #'enable-viper-lisp-mode))
        '(emacs-lisp-mode-hook
          lisp-mode-hook
          lisp-interaction-mode-hook
          scheme-mode-hook
          clojure-mode-hook))

  (define-key viper-vi-global-user-map [(control ?t)]
    #'(lambda ()
        (interactive)
        (if viper-lisp-mode
            (transpose-sexps 1)
          (transpose-words 1))))

  (when (require 'paredit nil t)
    (require 'viper-paredit))

  (define-viper-lisp-brac-function ?\j
    #'(lambda (arg)
        (down-list (- (or arg 1)))))

  (define-viper-lisp-brac-function ?\k
    #'(lambda (arg)
        (up-list (- (or arg 1)))))

  (define-viper-lisp-ket-function ?\j
    #'(lambda (arg)
        (down-list arg)))

  (define-viper-lisp-ket-function ?\k
    #'(lambda (arg)
        (up-list arg)))

  (define-viper-lisp-brac-function ?y
    #'(lambda (arg)
        (pop-tag-mark)))

  (define-viper-lisp-ket-function ?y
    #'(lambda (arg)
        (find-tag (car (find-tag-interactive "Find tag: ")))))
  )
