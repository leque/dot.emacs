(el-get-bundle lice)

(defun change-frame-width (w)
  (interactive "nnew width: ")
  (set-frame-width (selected-frame) w))
