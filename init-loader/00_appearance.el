;;; CUD colorset ver. 3 http://jfly.iam.u-tokyo.ac.jp/colorset/
;; accent colors
(defvar my-cud-color-red           "#ff2800")
(defvar my-cud-color-yellow        "#faf500")
(defvar my-cud-color-green         "#35a16b")
(defvar my-cud-color-blue          "#0041ff")
(defvar my-cud-color-skyblue       "#66ccff")
(defvar my-cud-color-pink          "#ff99a0")
(defvar my-cud-color-orange        "#ff9900")
(defvar my-cud-color-purple        "#9a0079")
(defvar my-cud-color-brown         "#663300")
;; base colors
(defvar my-cud-color-light-pink    "#ffd1d1")
(defvar my-cud-color-cream         "#ffff99")
(defvar my-cud-color-lime          "#cbf266")
(defvar my-cud-color-light-skyblue "#b4ebfa")
(defvar my-cud-color-beige         "#edc58f")
(defvar my-cud-color-light-green   "#87e7b0")
(defvar my-cud-color-light-purple  "#c7b2de")
;; achromatic colors
(defvar my-cud-color-white         "#ffffff")
(defvar my-cud-color-light-gray    "#c8c8cb")
(defvar my-cud-color-gray          "#7f878f")
(defvar my-cud-color-black         "#000000")

(require 'font-lock)
(global-font-lock-mode +1)

(require 'whitespace)
(setq whitespace-line-column my-frame-width)
(setq whitespace-style '(face tabs trailing lines-tail))
(global-whitespace-mode +1)

(require 'paren)
(show-paren-mode +1)
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match
                    nil
                    :inherit 'highlight
                    :background nil
                    )
(set-face-attribute 'show-paren-mismatch
                    nil
                    :foreground "white"
                    :background my-cud-color-red)

(el-get-bundle! diminish)

(defmacro my-diminish (&rest modes)
  `(progn ,@(cl-loop for (feature mode) in modes
                     collect `(with-eval-after-load-feature ',feature
                                (diminish ',mode)))))

(my-diminish
 (git-gutter git-gutter-mode)
 (indent-guide indent-guide-mode)
 (ivy ivy-mode)
 (whitespace global-whitespace-mode)
 (zoom zoom-mode)
 )

(el-get-bundle! dimmer
  :type elpa
  :repo ("melpa" . "https://melpa.org/packages/")
  (dimmer-mode)
  (with-eval-after-load-feature 'dimmer
    (setq dimmer-fraction 0.3)))

(el-get-bundle rainbow-mode)

(el-get-bundle! symbol-overlay
  (with-eval-after-load-feature 'symbol-overlay
    (cl-loop for color in (list
                           my-cud-color-light-pink
                           my-cud-color-cream
                           my-cud-color-lime
                           my-cud-color-light-skyblue
                           my-cud-color-beige
                           my-cud-color-light-skyblue
                           my-cud-color-light-green
                           my-cud-color-light-purple
                           )
             for i = 1 then (+ i 1)
             while (<= i (length symbol-overlay-faces))
             do (set-face-background (intern (format "symbol-overlay-face-%d" i))
                                     color))
    (set-face-background 'symbol-overlay-default-face
                         my-cud-color-light-purple))
  (add-hook 'prog-mode-hook #'symbol-overlay-mode))

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
