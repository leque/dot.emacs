(el-get-bundle lice
  (with-eval-after-load-feature 'lice
    (defadvice lice:insert-copyright (around
                                      override-lice:insert-copyright
                                      activate)
      (insert (format "Copyright (c) %s %s\n\n"
                      (format-time-string "%Y")
                      (user-full-name))))
    ))

(defun change-frame-width (w)
  (interactive "nnew width: ")
  (set-frame-width (selected-frame) w))