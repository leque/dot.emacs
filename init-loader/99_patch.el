(with-eval-after-load-feature 'elp
  ;; monkey patch to avoid
  ;; `cl--generic-standard-method-combination: Unsupported qualifiers in function loadhist-unload-element: (:before :extra "elp")`
  ;; which is fixed in (>= 28.1):
  ;; https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=51e78e91464601fc9adef1ca9c1c5ff0a23043ef
  (let ((gf (cl--generic 'loadhist-unload-element)))
    (setf (cl--generic-method-table gf)
          (cl-loop for meth in (cl--generic-method-table gf)
                   unless (equal (cl--generic-method-qualifiers meth) '(:before :extra "elp"))
                   collect meth))
    ;; copied and modified from emacs-lisp/elp.el
    ;; `:extra NAME` qualifier must place before other qualifiers?
    (cl-defmethod loadhist-unload-element :extra "elp" :before ((x (head defun)))
      "Un-instrument before unloading a function."
      (elp-restore-function (cdr x))))
  )
