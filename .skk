;; -*- mode: emacs-lisp, coding: utf-8 -*-

(setq skk-server-host "localhost")
(setq skk-server-portnum 1178)

(setq skk-large-jisyo (locate-file "SKK-JISYO.L" (list user-emacs-directory)))

(setq skk-use-viper t)
(viper-harness-minor-mode "skk-annotation")

(setq skk-auto-start-henkan nil)
(setq skk-henkan-strict-okuri-precedence t)
(setq skk-show-annotation t)

(defadvice skk-num-to-kanji (after type3-zero-to-rei)
  "#3 型数値変換で `0' を `〇' でなく `零' に変換する"
  (let ((type (ad-get-arg 1)))
    (when (and (eq type 'type3)
               (string-equal ad-return-value "〇"))
      (setq ad-return-value "零"))))

(require 'seikana-ziom)

(define-key skk-j-mode-map (kbd "C-c C-h")
  #'seikana-display-ziom-for-char-at-point)

(require 'skk-kanagaki-util)
(require 'skk-pskana)

(setq skk-kanagaki-dakuten-alist
      (append skk-kanagaki-dakuten-alist '(("ゝ" "ゞ")
                                           ("ヽ" "ヾ"))))

;;; * 月配列2-263の右手薬指以右の異手シフトを
;;;     や ぇ 「
;;;     わ ゆ[」]
;;;    [ー]ぉ
;;;   から
;;;     や ぇ 「[」]
;;;     わ ゆ[ー]
;;;    [？]ぉ
;;;   に変更し、さらに、「ー」の異手ダブルシフトを『〜』に
;;; * 同手シフトに追加のかな
;;; * 異手ダブルシフトにたまご風の矢印・記号
(defconst my-skk-rom-kana-rule-list-qwerty
  '(("\C-k" nil skk-toggle-kana)
    ("\C-l" nil skk-latin-mode)
    ("kj" nil skk-jisx0208-latin-mode)
    ("!" nil "！")
    ("(" nil "（")
    (")" nil "）")
    ("d." nil "？")
    ("ds" nil ("ヮ" . "ゎ"))
    ("ki" nil ("ヰ" . "ゐ"))
    ("ko" nil ("ヱ" . "ゑ"))
    ("km" nil "々")
    ("kn" nil ("ヽ" . "ゝ"))
    ("ddh" nil "←")
    ("ddj" nil "↓")
    ("ddk" nil "↑")
    ("ddl" nil "→")
    ("dd," nil "‥")
    ("dd." nil "…")
    ("dd/" nil "・")
    ("dd " nil "　")
    ))

(defconst my-skk-rom-kana-rule-list-us
  `(,@my-skk-rom-kana-rule-list-qwerty
    ("d[" nil "「")
    ("d]" nil "」")
    ("d'" nil "ー")
    ("dd[" nil "『")
    ("dd]" nil "』")
    ("dd'" nil "〜")
    ("\\" nil ("チ" . "ち"))          ; for kinesis ergonomic keyboard
    ))

(defconst my-skk-rom-kana-rule-list-jis
  `(,@my-skk-rom-kana-rule-list-qwerty
    ("d@" nil "「")
    ("d[" nil "」")
    ("d:" nil "ー")
    ("dd@" nil "『")
    ("dd[" nil "』")
    ("dd:" nil "〜")
    ))

(cl-case skk-pskana-keyboard-layout
  ((us)
   (setq skk-rom-kana-rule-list my-skk-rom-kana-rule-list-us)
   (setq skk-downcase-alist
         (cons '(?\| . ?\\)
               skk-downcase-alist))
   (setq skk-set-henkan-point-key
         (cons ?\| skk-set-henkan-point-key)))
  ((jis)
   (setq skk-rom-kana-rule-list my-skk-rom-kana-rule-list-jis))
  )
