(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

(setq inhibit-startup-message t)
(setq initial-frame-alist '())
(setq make-backup-files nil)
(setq require-final-newline t)
(setq default-frame-alist
      `((width . 80)
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

(el-get-bundle csv-mode
  (with-eval-after-load-feature 'csv-mode
    (autoload 'csv-auto-align-fields-minior-mode "csv-auto-align-fields")
    (add-hook 'csv-mode-hook 'csv-auto-align-fields-minior-mode)
    ))

(custom-set-variables
 '(csv-invisibility-default nil)
 '(csv-align-style 'right)
 )

(el-get-bundle indent-tools
  :type elpa
  :repo ("melpa" . "https://melpa.org/packages/")
  :depends (s hydra yafolding)
  )
