(el-get-bundle! auto-complete
  (ac-config-default)
  (setq ac-auto-start nil)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  )

(el-get-bundle! swiper
  (el-get-bundle! flx)
  (ivy-mode +1)
  (global-set-key (kbd "M-x") #'counsel-M-x)
  (global-set-key (kbd "C-x C-f") #'counsel-find-file)
  (with-eval-after-load-feature 'ivy
    (setq ivy-display-style 'fancy)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-initial-inputs-alist '())
    (setq ivy-re-builders-alist
          '((t . ivy--regex-fuzzy)))
    (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-partial)
    ))
