(require 'paredit)
(require 'gauche-mode)

(defvar gauche-paredit-paren-prefix-pat
  (mapconcat
   #'identity
   '(
     "#[suf]\\(8\\|16\\|32\\|64\\)"     ; SRFI-4
     "#[0-9]+="                         ; SRFI-38
     "(\\^"                             ; (^(x y) ...)
     "#\\?="                            ; debug-print
     "#vu8"                             ; R6RS bytevector
     )
   "\\|"))

(defun gauche-paredit-space-for-delimiter-p (endp delimiter)
  (or endp
      (if (= (char-syntax delimiter) ?\()
          (not (looking-back gauche-paredit-paren-prefix-pat))
        t)))

(defun gauche-paredit-in-regexp-p ()
  (and (paredit-in-string-p)
       (= ?\/ (char-after (car (paredit-string-start+end-points))))))

(defun gauche-paredit-slash (&optional n)
  (interactive "P")
  (cond ((gauche-paredit-in-regexp-p)
         (if (= (point) (cdr (paredit-string-start+end-points)))
             (forward-char)
           (insert ?\\ ?\/)))
        ((paredit-in-comment-p)
         (insert ?\/))
        ((not (paredit-in-char-p))
         (if (= (char-before) ?\#)
             (paredit-insert-pair n ?\/ ?\/ 'paredit-forward-for-quote)
           (insert ?\/)))))

(add-hook 'scheme-mode-hook
            #'(lambda ()
                (set (make-local-variable
                      'paredit-space-for-delimiter-predicates)
                     (list #'gauche-paredit-space-for-delimiter-p))))

(define-key gauche-mode-map "/" #'gauche-paredit-slash)

(provide 'gauche-paredit)
