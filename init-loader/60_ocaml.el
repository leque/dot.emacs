(el-get-bundle caml-mode
  (with-eval-after-load-feature 'caml
    (require 'caml-font))
  )

(el-get-bundle dune-mode
  (push `(,(rx (or (seq "jbuild"
                        (opt ".inc"))
                   "dune-project"
                   "dune-workspace"
                   )
               eos)
          . dune-mode)
        auto-mode-alist))
