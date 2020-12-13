(el-get-bundle! git-gutter
  (global-git-gutter-mode)
  (set-face-foreground 'git-gutter:added my-cud-color-green)
  (set-face-foreground 'git-gutter:deleted my-cud-color-red)
  (set-face-foreground 'git-gutter:modified my-cud-color-pink)

  (setq git-gutter:modified-sign "█"
        git-gutter:added-sign "▌"
        git-gutter:deleted-sign "▐"
        )
  )
