(require 'font-lock)
(global-font-lock-mode +1)

(require 'whitespace)
(setq whitespace-style '(face tabs trailing lines-tail))
(global-whitespace-mode +1)

(el-get-bundle rainbow-mode)

(el-get-bundle! symbol-overlay
  (setq symbol-overlay-colors
        '("#ffd1d1"
          "#ffff99"
          "#cbf266"
          "#b4ebfa"
          "#edc58f"
          "#87e7b0"
          "#c7b2de"
          ))
  (set-face-background 'symbol-overlay-temp-face
                       (car (last symbol-overlay-colors)))
  (add-hook 'prog-mode-hook #'symbol-overlay-mode))

(el-get-bundle! mic-paren
  (paren-activate)
  (setq paren-match-face 'region)
  (setq paren-sexp-mode t))

(el-get-bundle! indent-guide
  (indent-guide-global-mode)
  (with-eval-after-load-feature 'indent-guide
    (setq indent-guide-recursive t)
    (set-face-foreground 'indent-guide-face "#ddd")
    (dolist (x '(Info-mode))
      (cl-pushnew x indent-guide-inhibit-modes))
    ))

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

