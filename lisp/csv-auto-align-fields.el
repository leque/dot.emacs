(require 'csv-mode)
(require 'cl-lib)

;;;###autoload
(define-minor-mode csv-auto-align-fields-minior-mode
  "automatically call `csv-align-fields'."
  :init-value nil
  :lighter " AA"
  (cl-callf2 (lambda (fn fns)
               (cond (csv-auto-align-fields-minior-mode
                      (csv-align-fields nil (point-min) (point-max))
                      (cl-adjoin fn fns))
                     (t
                      (remove fn fns))))
      'csv-auto-align-fields
      (buffer-local-value 'after-change-functions (current-buffer))))

(defun csv-auto-align-fields (_beg _end _len)
  (csv-align-fields nil (window-start) (window-end)))

(provide 'csv-auto-align-fields)
