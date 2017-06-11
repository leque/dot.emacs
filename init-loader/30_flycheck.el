(el-get-bundle flycheck
  (with-eval-after-load-feature 'flycheck
    (setq flycheck-display-errors-function
          (lambda (errors)
            (let ((messages (mapcar #'flycheck-error-message errors)))
              (popup-tip (string-join messages "\n")))))
    (setq flycheck-display-errors-delay 0.5)
    (define-key flycheck-mode-map (kbd "C-M-n") 'flycheck-next-error)
    (define-key flycheck-mode-map (kbd "C-M-p") 'flycheck-previous-error)
    ))
