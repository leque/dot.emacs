(defun org-preview-html/export-github-markup-as-html (filename)
  (call-process "github-markup"
                nil
                `(:file ,filename)
                nil
                (buffer-file-name)))

(el-get-bundle leque/org-preview-html
  :branch "other-formats"
  (with-eval-after-load-feature 'org-preview-html
    (push (cons (rx "."
                    (or
                     (or "markdown" "mdown" "mkdn" "md")
                     (or "rst")
                     (or "asciidoc" "adoc" "asc")
                     )
                    eos)
                #'org-preview-html/export-github-markup-as-html)
          org-preview-html/export-html-function-alist)
    ))

(el-get-bundle smaximov/org-commentary
  :prepare (progn
             (autoload 'org-commentary-update "org-commentary"
               "Update library headers using the content of an Org document."
               t)
             )
  )
