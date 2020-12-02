(require 'font-lock)
(global-font-lock-mode +1)

(require 'whitespace)
(setq whitespace-line-column my-frame-width)
(setq whitespace-style '(face tabs trailing lines-tail))
(global-whitespace-mode +1)

(show-paren-mode +1)
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match
                    nil
                    :inherit 'region
                    :background nil
                    )
(set-face-attribute 'show-paren-mismatch
                    nil
                    :foreground "white"
                    :background "purple")

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
