(with-eval-after-load-feature 'shr
  (defun shr-tag-code (dom)
    (let ((shr-current-font 'default))
      (shr-generic dom))))
