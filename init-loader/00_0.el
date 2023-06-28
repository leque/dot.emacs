(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

(defvar my-frame-width 100)

(setq inhibit-startup-message t)
(setq initial-frame-alist '())
(setq make-backup-files nil)
(setq require-final-newline t)
(setq default-frame-alist
      `((width . ,my-frame-width)
        (menu-bar-lines . ,(if window-system 1 0))
        (tool-bar-lines . nil)
        (line-spacing . 1)
        ))
(setq initial-frame-alist
      `((left . (- 0))
        (user-position . t)
        (fullscreen . fullheight)))

(setq-default indent-tabs-mode nil)
(setq-default indicate-buffer-boundaries t)

(blink-cursor-mode t)
(transient-mark-mode nil)

(setq-default dired-bind-jump nil)

;;; key bindings
(global-set-key (kbd "C-h") #'backward-delete-char)
(global-set-key (kbd "C-x h") #'help)

(el-get-bundle dash)
(el-get-bundle! s)
(el-get-bundle! request)
(el-get-bundle! popup)
(el-get-bundle xmlgen)
(el-get-bundle mattiase/xr)
(el-get-bundle elpa:keypression)
(el-get-bundle! which-key
  (with-eval-after-load-feature 'which-key
    (setq which-key-frame-max-height 40)
    (which-key-mode)))

(el-get-bundle leque/csv-mode
  :checkout "dev"
  (with-eval-after-load-feature 'csv-mode
    (add-hook 'csv-mode-hook 'csv-align-fields-mode)
    (autoload 'my-csv-open-column "my-csv")
    (add-hook 'csv-mode-hook
              (lambda ()
                (define-key csv-mode-map (kbd "C-c C-o") 'my-csv-open-column)))
    ))

(custom-set-variables
 '(csv-invisibility-default nil)
 '(csv-align-style 'auto)
 `(httpd-root ,(locate-user-emacs-file "www"))
 )

(el-get-bundle indent-tools
  :type elpa
  :repo ("melpa" . "https://melpa.org/packages/")
  :depends (s hydra yafolding)
  )
