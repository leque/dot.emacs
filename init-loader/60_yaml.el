(el-get-bundle 'yaml-mode
  (with-eval-after-load-feature 'yaml-mode
    (with-eval-after-load-feature 'indent-tools
      (defun yaml-indent-tools-mode ()
        (define-key yaml-mode-map
          indent-tools-keymap-prefix #'indent-tools-hydra/body))
      (add-hook 'yaml-mode-hook 'yaml-indent-tools-mode))))
