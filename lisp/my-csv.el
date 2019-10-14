(require 'csv-mode)
(require 'cl-lib)

(defun my-csv-current-column ()
  (interactive)
  (save-excursion
    (let* ((cur (point))
           (pos (progn
                  (beginning-of-line)
                  (if (looking-at-p csv-separator-regexp)
                      0
                    -1)))
           (end (line-beginning-position 2)))
      (while (< (point) cur end)
        (csv-forward-field 1)
        (cl-incf pos))
      (max 0 pos))))

;;;###autoload
(defun my-csv-open-column (n)
  "Insert a new column after the current column.
With arg N, insert N columns.
If N is negative, insert N columns before the current column."
  (interactive "p")
  (save-excursion
    (let ((col (my-csv-current-column))
          (com (regexp-quote csv-comment-start)))
      (goto-char (point-min))
      (while (< (point) (1- (point-max)))
        (unless (looking-at-p com)
          (let ((end (line-beginning-position 2)))
            (csv-forward-field (max 0
                                    (+ col
                                       (if (looking-at-p csv-separator-regexp)
                                           -1 0)
                                       (if (< n 0)
                                           0 1))))
            (goto-char (min (point) end))
            (dotimes (_ (abs n))
              (insert (car csv-separators)))))
        (forward-line)))))

(provide 'my-csv)
