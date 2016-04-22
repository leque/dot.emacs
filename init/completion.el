(el-get-bundle! auto-complete
  (ac-config-default)
  (setq ac-auto-start nil)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  )
