(defvar my-syntax-highlighter-url-prefix
  "https://cdnjs.cloudflare.com/ajax/libs/prism/1.22.0")

(defvar my-syntax-highlighter-stylesheets
  `(
    (link :href ,(concat my-syntax-highlighter-url-prefix
                         "/themes/prism.min.css")
          :type "text/css"
          :rel "stylesheet")
    ))

(defvar my-syntax-highlighter-scripts
  `(
    (script :src ,(concat my-syntax-highlighter-url-prefix
                          "/components/prism-core.min.js")
            :data-manual ""
            "")
    (script :src ,(concat my-syntax-highlighter-url-prefix
                          "/plugins/autoloader/prism-autoloader.min.js")
            "")
    ))

(defun my-github-markup-filter (buffer)
  "Convert contents of BUFFER to HTML by using github-markup (URL `https://github.com/github/markup')."
  (let* ((filename (buffer-file-name buffer))
         status
         stderr
         error
         (output (let ((err-file (make-temp-file "github-markup-err")))
                   (unwind-protect
                       (condition-case err
                           (with-output-to-string
                             (setq status
                                   (call-process "github-markup"
                                                 nil
                                                 (list standard-output err-file)
                                                 nil
                                                 filename))
                             (setq stderr
                                   (with-temp-buffer
                                     (insert-file-contents err-file)
                                     (buffer-string))))
                         (error
                          (setq error err)
                          nil))
                     (delete-file err-file)))))
    (insert
     (xmlgen
      `(html
        (head
         (title ,(or filename (buffer-name buffer)))
         ,@my-syntax-highlighter-stylesheets
         ;; https://github.com/sindresorhus/github-markdown-css
         ;; https://cdnjs.com/libraries/github-markdown-css
         (link :rel "stylesheet"
               :type "text/css"
               :href "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.10.0/github-markdown.min.css")
         (link :rel "stylesheet"
               :type "text/css"
               :href "/css/github-markup-preview.css")
         (script :src "/js/github-markup-preview.js"
                 "")
         )
        (body
         ,@(when error
             `((pre :class "error" ,(format "%S" error))))
         ,@(when (s-present? stderr)
             `((pre :class "stderr" ,stderr)))
         ,@(when (eql status 0)
             `((div :class "markdown-body" (!unescape ,output))))
         ,@my-syntax-highlighter-scripts
         ))))))

(defun my-asciidoctor-js-filter (buffer)
  "Convert contents of BUFFER to HTML by using asciidocor.js (URL `https://github.com/asciidoctor/asciidoctor.js/')."
  (insert
   (xmlgen
    `(html
      (head
       (title ,(or (buffer-file-name buffer) (buffer-name buffer)))
       ,@my-syntax-highlighter-stylesheets
       (link :rel "stylesheet"
             :type "text/css"
             :href "/js/node_modules/@asciidoctor/core/dist/css/asciidoctor.css")
       (link :rel "stylesheet"
             :type "text/css"
             :href "/css/asciidoc-preview.css")
       (script :src "/js/node_modules/@asciidoctor/core/dist/browser/asciidoctor.js" "")
       (script :src "/js/asciidoc-preview.js" "")
       )
      (body
       (div :id "content" ,(with-current-buffer buffer (buffer-string)))
       (div :id "body")
       ,@my-syntax-highlighter-scripts
       )))))

(define-derived-mode my-doc-mode text-mode "my-doc")

(add-to-list 'auto-mode-alist
             `(,(rx "."
                    (or (or "md" "markdown")
                        (or "adoc" "asciidoc")
                        "txt")
                    eos)
               . my-doc-mode))

(el-get-bundle impatient-mode
  (declare-function imp-set-user-filter "impatient-mode")
  (cl-loop for mode-hook in '(my-doc-mode-hook)
           do (add-hook mode-hook
                        (lambda ()
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
