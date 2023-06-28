(el-get-bundle lice
  :type elpa
  :repo ("melpa" . "https://melpa.org/packages/")
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

(defun my-align-frame-top-right ()
  (interactive)
  (set-frame-position
   (selected-frame)
   (cadr (frame-geom-value-cons 'left '(- 0)))
   0))

(defun my-string-find-all-matches (regex string)
  "Return a list of matches for REGEX in STRING.

Each element itself is a list of matches, as per
`match-string'. Multiple matches at the same position will be
ignored after the first."
  (declare (side-effect-free t))
  (save-match-data
    (let ((all-strings ())
          (i 0))
      (while (and (< i (length string))
                  (string-match regex string i))
        (setq i (let ((e (match-end 0)))
                  (if (= e i)
                      (1+ e)
                    e)))
        (let ((num-matches (/ (length (match-data)) 2)))
          (push (cl-loop for match from 0 below num-matches
                         collect (match-string match string))
                all-strings)))
      (nreverse all-strings))))
