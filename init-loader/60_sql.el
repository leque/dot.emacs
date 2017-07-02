(el-get-bundle alex-hhh/emacs-sql-indent
  :description "Syntax based indentation for SQL files inside GNU Emacs"
  :type github
  :features sql-indent
  (with-eval-after-load-feature 'sql-indent
    (add-hook 'sql-mode-hook #'sqlind-minor-mode)
    ))
