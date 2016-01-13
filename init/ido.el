(ido-mode +1)

(with-eval-after-load-feature 'ido
  (ido-everywhere +1)
  (setq ido-enable-flex-matching t)
  (setq ido-confirm-unique-completion t)
  )

(el-get-bundle ido-vertical-mode
  (ido-vertical-mode +1)
  (with-eval-after-load-feature 'ido-vertical-mode
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)
    ))

(el-get-bundle smex
  (global-set-key (kbd "M-x") 'smex))

(el-get-bundle ido-ubiquitous
  (ido-ubiquitous-mode +1))
