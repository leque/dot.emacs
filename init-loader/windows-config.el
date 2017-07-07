(let ((font (font-spec :name "MyricaM M"))
      (font-size 120))
  (when (find-font font)
    (set-face-attribute 'default nil
                        :family font
                        :height font-size)
    ))
