(el-get-bundle! git-gutter
  (global-git-gutter-mode)
  (set-face-foreground 'git-gutter:added my-cud-green-color)
  (set-face-foreground 'git-gutter:deleted my-cud-red-color)
  (set-face-foreground 'git-gutter:modified my-cud-pink-color)

  (setq git-gutter:modified-sign "█"
        git-gutter:added-sign "▌"
        git-gutter:deleted-sign "▐"
        )
  )
