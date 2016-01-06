(with-eval-after-load-feature 'cc-mode
  (c-add-style "ruby"
               '("bsd"
                 (c-basic-offset . 4)
                 (c-offsets-alist
                  (case-label . 2)
                  (label . 2)
                  (statement-case-intro . 2)
                  (statement-case-open . 2))))

  (c-add-style "gauche"
               '("bsd"
                 (c-basic-offset . 4)
                 (c-offsets-alist
                  (label . 2)
                  (statement-case-open . 4))))
  )
