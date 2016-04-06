(cond
 ((version< emacs-version "24.4")
  (el-get-bundle "magit1"))
 (t
  (el-get-bundle "magit")))

