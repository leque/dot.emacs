(setq ns-command-modifier 'meta)
(setq ns-option-modifier 'super)

(setq dired-use-ls-dired nil)

(let ((size 14)
      (ascii-font "Monaco")
      (ja-font "Hiragino Maru Gothic Pro")
      (hg-font "UniHentaiKana"))
  (set-face-attribute 'default nil
                      :family ascii-font
                      :height (* size 10))
  (mapc (lambda (x)
          (set-fontset-font t x ja-font))
        '(katakana-jisx0201
          japanese-jisx0208
          japanese-jisx0212
          japanese-jisx0213-1
          japanese-jisx0213-2
          ))
  (set-fontset-font t
                    (cons (decode-char 'ucs #x1b001)
                          (decode-char 'ucs #x1B11E))
                    hg-font)
  (set-fontset-font t '(#x0080 . #x024F) ascii-font))

(setq frame-inherited-parameters '(font tool-bar-lines))

(add-to-list 'face-font-rescale-alist
             '(".*Hiragino.*" . 1.2))
