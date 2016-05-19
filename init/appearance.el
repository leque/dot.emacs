(require 'font-lock)
(global-font-lock-mode +1)

(require 'whitespace)
(setq whitespace-style '(face tabs trailing lines-tail))
(global-whitespace-mode +1)

(el-get-bundle! mic-paren
  (paren-activate)
  (setq paren-match-face 'region)
  (setq paren-sexp-mode t))

(custom-set-faces
 '(whitespace-line
   ((t (
        :inherit t
        :background "gray85"))))
 ;; avoid collision with ivy-minibuffer-faces
 '(viper-minibuffer-insert
   ((t (
        :inherit t
        :background nil))))
 )

