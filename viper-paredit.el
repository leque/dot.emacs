(require 'viper-lisp)

;; for now, do not use replace overlay with paredit.
(defadvice viper-change (around viper-change-with-paredit activate)
  "delete regions on cw, c), c, s, etc. in paredit-mode"
  (if (or (not paredit-mode)
          (paredit-in-char-p)
          (paredit-in-string-p)
          (paredit-in-comment-p))
      ad-do-it
    (flet ((viper-same-line (_beg _end) nil))
      ad-do-it)))

(defadvice viper-set-destructive-command
  (before viper-set-destructive-command-with-paredit activate)
  "make 3s etc. works correctly with paredit"
  (let ((arg (ad-get-arg 0)))
    (when (and paredit-mode
               (not (or (paredit-in-char-p)
                        (paredit-in-string-p)
                        (paredit-in-comment-p)))
               (eq (car arg) 'viper-substitute))
      (setcar (cdr arg) 1))))

(define-key viper-insert-global-user-map [(ctrl ?\h)]
  #'(lambda ()
      (interactive)
      (if viper-lisp-mode
          (paredit-backward-delete)
        (viper-del-backward-char-in-insert))))

(provide 'viper-paredit)
