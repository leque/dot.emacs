(with-eval-after-load-feature 'cc-mode
  (c-add-style "ruby"
               '("bsd"
                 (c-basic-offset . 4)
                 (c-offsets-alist
                  (case-label . 2)
                  (label . 2)
                  (statement-case-intro . 2)
                  (statement-case-open . 2))))
  (c-add-style "gauche"
               '("bsd"
                 (c-basic-offset . 4)
                 (c-offsets-alist
                  (label . 2)
                  (statement-case-open . 4))))
  )

(let ((hooks '(c-mode-hook c++-mode-hook objc-mode-hook)))
  (el-get-bundle irony-mode
    (dolist (hook hooks)
      (add-hook hook 'flycheck-mode)
      (add-hook hook 'irony-mode))
    (with-eval-after-load-feature 'irony
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
      (el-get-bundle! ac-irony
        (add-to-list 'ac-sources 'ac-source-irony))
      (el-get-bundle! flycheck-irony
        (flycheck-irony-setup))
      ))

  (el-get-bundle rtags
    (with-eval-after-load-feature 'rtags
      (dolist (hook hooks)
        (add-hook hook
                  (lambda ()
                    (when (rtags-is-indexed)
                      (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
                      (local-set-key (kbd "M-;") 'rtags-find-symbol)
                      (local-set-key (kbd "M-@") 'rtags-find-references)
                      (local-set-key (kbd "M-,") 'rtags-location-stack-back)))))
      )))
