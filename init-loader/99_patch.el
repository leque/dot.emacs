(with-eval-after-load-feature 'elp
   ;; monkey patch to avoid
   ;; `cl--generic-standard-method-combination: Unsupported qualifiers in function loadhist-unload-element: (:before :extra "elp")`
  (let ((gf (cl--generic 'loadhist-unload-element)))
    (setf (cl--generic-method-table gf)
          (loop for meth in (cl--generic-method-table gf)
                unless (equal (cl--generic-method-qualifiers meth) '(:before :extra "elp"))
                collect meth))
    ;; copied and modified from emacs-lisp/elp.el
    ;; `:extra NAME` qualifier must place before other qualifiers?
    (cl-defmethod loadhist-unload-element :extra "elp" :before ((x (head defun)))
      "Un-instrument before unloading a function."
      (elp-restore-function (cdr x))))
  )
