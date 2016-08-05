(el-get-bundle ensime)

(el-get-bundle scala-mode
  (with-eval-after-load-feature 'scala-mode
    (setq scala-indent:use-javadoc-style t)
    (setq scala-indent:add-space-for-scaladoc-asterisk nil)
    ))
