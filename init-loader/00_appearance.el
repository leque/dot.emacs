(defun my-rgb (spec)
  (cond ((string-match (rx bos
                           "rgb(" (* space)
                           (submatch-n 1 (** 1 3 digit))
                           (* space) "," (* space)
                           (submatch-n 2 (** 1 3 digit))
                           (* space) "," (* space)
                           (submatch-n 3 (** 1 3 digit))
                           (* space) ")"
                           eos)
                       spec)
         (format "#%02x%02x%02x"
                 (string-to-number (match-string 1 spec))
                 (string-to-number (match-string 2 spec))
                 (string-to-number (match-string 3 spec))
                 ))))

;;; CUD colorset ver. 4 http://jfly.iam.u-tokyo.ac.jp/colorset/
;; accent colors
(defvar my-cud-color-red           (my-rgb "rgb(255,  75,   0)"))
(defvar my-cud-color-yellow        (my-rgb "rgb(255, 241,   0)"))
(defvar my-cud-color-green         (my-rgb "rgb(  3, 175, 122)"))
(defvar my-cud-color-blue          (my-rgb "rgb(  0,  90, 255)"))
(defvar my-cud-color-skyblue       (my-rgb "rgb( 77, 196, 255)"))
(defvar my-cud-color-pink          (my-rgb "rgb(255, 128, 130)"))
(defvar my-cud-color-orange        (my-rgb "rgb(246, 170,   0)"))
(defvar my-cud-color-purple        (my-rgb "rgb(153,   0, 153)"))
(defvar my-cud-color-brown         (my-rgb "rgb(128,  64,   0)"))
;; base colors
(defvar my-cud-color-light-pink    (my-rgb "rgb(255, 202, 191)"))
(defvar my-cud-color-cream         (my-rgb "rgb(255, 255, 128)"))
(defvar my-cud-color-lime          (my-rgb "rgb(216, 242,  85)"))
(defvar my-cud-color-light-skyblue (my-rgb "rgb(191, 228, 255)"))
(defvar my-cud-color-beige         (my-rgb "rgb(255, 202, 128)"))
(defvar my-cud-color-light-green   (my-rgb "rgb(119, 217, 168)"))
(defvar my-cud-color-light-purple  (my-rgb "rgb(201, 172, 230)"))
;; achromatic colors
(defvar my-cud-color-white         (my-rgb "rgb(255, 255, 255)"))
(defvar my-cud-color-light-gray    (my-rgb "rgb(200, 200, 203)"))
(defvar my-cud-color-gray          (my-rgb "rgb(132, 145, 158)"))
(defvar my-cud-color-black         (my-rgb "rgb(  0,   0,   0)"))

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
    (setq dimmer-fraction 0.5)))

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

;; Local Variables:
;; rainbow-html-colors: t
;; rainbow-x-colors: nil
;; eval: (rainbow-mode)
;; End:
