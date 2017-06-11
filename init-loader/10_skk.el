(setq-default skk-init-file (locate-user-emacs-file ".skk.el"))

(el-get-bundle ddskk
  (global-set-key (kbd "C-x C-j") #'skk-mode))

(el-get-bundle nicola-ddskk)
