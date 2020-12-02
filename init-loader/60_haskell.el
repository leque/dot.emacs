(el-get-bundle haskell-mode
  :type elpa
  :repo ("melpa" . "https://stable.melpa.org/packages/")
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))
