(require 'font-lock)
(global-font-lock-mode +1)

(require 'whitespace)
(setq whitespace-style '(face tabs trailing lines-tail))
(global-whitespace-mode +1)

(el-get-bundle rainbow-mode)

(defvar my-cud-red-color "#ff2800")
(defvar my-cud-yellow-color "#faf500")
(defvar my-cud-green-color "#35a16b")
(defvar my-cud-blue-color "#0041ff")
(defvar my-cud-skyblue-color "#66ccff")
(defvar my-cud-pink-color "#ff99a0")
(defvar my-cud-orange-color "#ff9900")
(defvar my-cud-purple-color "#9a0079")
(defvar my-cud-brown-color "#663300")
(defvar my-cud-light-pink-color "#ffd1d1")
(defvar my-cud-cream-color "#ffff99")
(defvar my-cud-lime-color "#cbf266")
(defvar my-cud-light-skyblue-color "#b4ebfa")
(defvar my-cud-beige-color "#edc58f")
(defvar my-cud-light-green-color "#87e7b0")
(defvar my-cud-light-purple-color "#c7b2de")
(defvar my-cud-light-gray-color "#c8c8cb")
(defvar my-cud-gray-color "#7f878f")

(el-get-bundle! symbol-overlay
  (setq symbol-overlay-colors
        (list
         my-cud-light-pink-color
         my-cud-cream-color
         my-cud-lime-color
         my-cud-light-skyblue-color
         my-cud-beige-color
         my-cud-light-skyblue-color
         my-cud-light-green-color
         my-cud-light-purple-color
         ))
  (with-eval-after-load-feature 'symbol-overlay
    (set-face-background 'symbol-overlay-default-face
                         my-cud-light-purple-color))
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

