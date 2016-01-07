;; -*- coding: utf-8 -*-
(require 'ert)
(require 'skk-pskana)
(require 'cl-lib)

(ert-deftest test-skk-pskana-init ()
  "定義済のキーボード配列、かな配列に対して skk-pskana-init が成功する"
  (cl-loop for kbd in '(us jis dvorak)
           for kana in '(tsuki-2-263 hana)
           do (progn
                (setq skk-pskana-keyboard-layout kbd)
                (setq skk-pskana-kana-layout kana)
                (skk-pskana-init))))
