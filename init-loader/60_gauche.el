(set-variable 'gauche-mode-info-language 'ja)

(el-get-bundle gauche-mode
  (push '("gosh" . (utf-8 . utf-8)) process-coding-system-alist)
  (with-eval-after-load-feature 'gauche-mode
    (add-hook 'gauche-mode-hook #'enable-gauche-paredit-mode)
    (add-hook 'gauche-mode-hook #'company-gauche-setup)
    ))

(with-eval-after-load-feature 'cmuscheme
  (defadvice scheme-send-region (after show-ischeme-buffer activate)
    "show *scheme* buffer always"
    (let ((buf (and scheme-buffer
                    (get-buffer scheme-buffer))))
      (when (cl-loop for frame in (frame-list)
                     never (cl-loop for w in (window-list frame)
                                    thereis (eq buf (window-buffer w))))
        (switch-to-buffer-other-window buf)
        (goto-char (point-max))
        (other-window 1))))
  )
