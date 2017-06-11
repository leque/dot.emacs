(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-l") 'windmove-right)

(global-set-key (kbd "s-H") 'shrink-window-horizontally)
(global-set-key (kbd "s-J") 'shrink-window)
(global-set-key (kbd "s-K") 'enlarge-window)
(global-set-key (kbd "s-L") 'enlarge-window-horizontally)

(el-get-bundle! buffer-move
  (global-set-key (kbd "M-g h") 'buf-move-left)
  (global-set-key (kbd "M-g j") 'buf-move-down)
  (global-set-key (kbd "M-g k") 'buf-move-up)
  (global-set-key (kbd "M-g l") 'buf-move-right))
