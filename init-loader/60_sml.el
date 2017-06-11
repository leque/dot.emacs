(el-get-bundle sml-mode
  (with-eval-after-load-feature 'sml-mode
    (setq sml-indent-level 2)
    (push '("\\.smi\\'" . sml-mode)
          auto-mode-alist)))
