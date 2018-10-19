(defun my-github-markup-filter (buffer)
  "Convert contents of BUFFER to HTML by using github-markup
\(URL `https://github.com/github/markup')."
  (let* ((filename (buffer-file-name buffer))
         st
         (output (with-output-to-string
                   (setq st
                         (call-process "github-markup"
                                       nil
                                       standard-output
                                       nil
                                       (buffer-file-name buffer))))))
    (insert "<html>")
    (insert (xmlgen
             `(head
               (title ,(or (buffer-file-name buffer)
                           (buffer-name buffer)))
               ;; https://github.com/sindresorhus/github-markdown-css
               ;; https://cdnjs.com/libraries/github-markdown-css
               (link :rel "stylesheet"
                     :type "text/css"
                     :href "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.10.0/github-markdown.min.css")
               (style :type "text/css"
                      "\
.markdown-body {
   box-sizing: border-box;
   min-width: 200px;
   max-width: 980px;
   margin: 0 auto;
   padding: 45px;
}
")
               )))
    (insert "<body>")
    (cond ((eql st 0)
           (insert "<div class=\"markdown-body\">")
           (insert output)
           (insert "</div>"))
          (t
           (insert "<pre>")
           (insert output)
           (insert "</pre>")))
    (insert "</body></html>")))

(add-to-list 'auto-mode-alist
             `(,(rx "."
                    (or (or "md" "markdown")
                        (or "adoc" "asciidoc")
                        "txt")
                    eos)
               . text-mode))

(el-get-bundle impatient-mode
  (declare-function imp-set-user-filter "impatient-mode")
  (cl-loop for mode-hook in '(text-mode-hook)
           do (add-hook mode-hook
                        #'(lambda ()
                            (impatient-mode)
                            (imp-set-user-filter #'my-github-markup-filter))))
  )

(el-get-bundle smaximov/org-commentary
  :prepare (progn
             (autoload 'org-commentary-update "org-commentary"
               "Update library headers using the content of an Org document."
               t)
             )
  )
