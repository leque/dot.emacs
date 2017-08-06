;;; skk-pskana.el --- Prefix Shift Kana Input Method for SKK  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2006-2017 OOHASHI, Daichi <dico.leque.comicron@gmail.com>

;; Author: OOHASHI, Daichi <leque@katch.ne.jp>
;; Package-Requires: ((ddskk "20160315.708") (dash "2.12.1"))
;; Keywords:

;; This file is *NOT* part of Daredevil SKK.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; skk-pskana.el
;; =============

;;   This is an utility to input Japanese with prefix shift kana layouts,
;;   such as

;;   - [花配列 (Hana layout)] (http://togasi.my.coocan.jp/hana_no_kuni/)
;;   - [月配列 2-263 (Tsuki-2-263 layout)]
;;     (http://jisx6004.client.jp/tsuki.html)
;;   - [Some other variants of Tsuki]
;;     (http://yellow.ribbon.to/%7Eujiro/hairetu.htm)

;;   We currently provide Hana layout and Tsuki-2-263 layout for
;;   US/JIS/Dvorak keyboards.


;; Requirements:
;; ~~~~~~~~~~~~~

;;   - [Daredevil SKK] (http://openlab.jp/skk/main-ja.html)
;;   - `skk-kanagaki-util.el' (in Daredevil SKK package)
;;   - [dash.el] (https://github.com/magnars/dash.el)


;; How to use
;; ~~~~~~~~~~

;;   Copy `skk-pskana.el' to a dicrectory in `load-path', and put in your
;;   `.skk' below:

;;   ,----
;;   | (require 'skk-pskana)
;;   `----

;;   The default layout is Tsuki-2-263 on US keyboards.  To use Hana layout
;;   on Dvorak keyboards, for example, put in your `.skk'

;;   ,----
;;   | (setq skk-pskana-keyboard-layout 'dvorak)
;;   | (setq skk-pskana-kana-layout 'hana)
;;   `----

;;   before loading `skk-pskana', or do `M-x skk-pskana-init' after
;;   setting.

;;   You would not have difficulty to use `skk-pskana' (at least once you
;;   have learned the layout, and are familiar to SKK), but there is a
;;   little bit difference from normal SKK to input an okurigana with
;;   dakuten/handakuten.

;;   On normal SKK, for instance, to input `遊ぶ', you have simply to type
;;   `AsoBu'. But here, you have to type `あそふ<Shift>゛': type a
;;   okurigana character without dakuten/handakuten, and hold down a Shift
;;   key and type dakuten/handakuten.

;;   I.e., with Tsuki-2-263, for `遊ぶ', you have to type `KfqkrL': `Kf'
;;   for `あ', `q' for `そ', `kr' for `ふ', and `L' for dakuten.  For `食べ
;;   る', type `GkxLm': `G' for `た', `kx' for `へ', `L' for dakuten, and
;;   `m' for `る'.


;; Note
;; ~~~~

;;   I use only Tsuki-2-263-us and Tsuki-2-263-jis, the others are not
;;   tested at all.  Use them at your own risk. The source is always with
;;   you.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'skk)
  (require 'skk-vars)
  (require 'skk-kanagaki-util)
  (require 'dash)
  (defvar skk-jisx0201-rule-list)
  (defvar skk-jisx0201-base-rule-list)
  )

(defgroup skk-pskana nil "SKK prefix shift kana input related customization."
  :prefix "skk-pskana-"
  :group 'skk-custom-by-filename)

(defcustom skk-pskana-keyboard-layout 'us
  "*使用するキーボードの物理配列"
  :type '(choice (const us)
                 (const jis)
                 (const dvorak)
                 (symbol :tag "Another Keyboard Type"))
  :group 'skk-pskana)

(defcustom skk-pskana-kana-layout 'tsuki-2-263
  "*使用するかな配列の種別"
  :type '(choice (const tsuki-2-263)
                 (const hana)
                 (symbol :tag "Another Keyboard Layout"))
  :group 'skk-pskana)

;;; Function Keys
(defconst skk-pskana-rom-kana-base-rule-list-hana-us
  '(("dq" nil skk-toggle-kana)
    ("kl" nil skk-latin-mode)
    ("ko" nil skk-jisx0208-latin-mode)
    ("#" nil skk-purge-from-jisyo)
    ("k/" nil skk-abbrev-mode)
    ("da" nil skk-set-henkan-point-subr)
    ("%" nil skk-input-by-code-or-menu)
    ("$" nil skk-display-code-for-char-at-point)
    ("&" nil skk-today)
    (skk-kakutei-key nil skk-kakutei)
    ))

(defconst skk-pskana-rom-kana-base-rule-list-tsuki-2-263-us
  skk-pskana-rom-kana-base-rule-list-hana-us)

(defconst skk-pskana-rom-kana-base-rule-list-hana-jis
  skk-pskana-rom-kana-base-rule-list-hana-us)

(defconst skk-pskana-rom-kana-base-rule-list-tsuki-2-263-jis
  skk-pskana-rom-kana-base-rule-list-tsuki-2-263-us)

(defconst skk-pskana-rom-kana-base-rule-list-hana-dvorak
  '(("eq" nil skk-toggle-kana)
    ("tl" nil skk-latin-mode)
    ("tr" nil skk-jisx0208-latin-mode)
    ("#" nil skk-purge-from-jisyo)
    ("t/" nil skk-abbrev-mode)
    ("ea" nil skk-set-henkan-point-subr)
    ("%" nil skk-input-by-code-or-menu)
    ("$" nil skk-display-code-for-char-at-point)
    ("&" nil skk-today)
    (skk-kakutei-key nil skk-kakutei)
    ))

(defconst skk-pskana-rom-kana-base-rule-list-tsuki-2-263-dvorak
  skk-pskana-rom-kana-base-rule-list-hana-dvorak)

(defconst skk-pskana-previous-candidate-keys-us
  '("-" "\C-p"))

(defconst skk-pskana-previous-candidate-keys-jis
  '("-" "\C-p"))

(defconst skk-pskana-previous-candidate-keys-dvorak
  '("[" "\C-p"))

(defconst skk-pskana-set-henkan-point-key-us
  '(?Q ?W ?E ?R ?T ?Y ?U ?I ?O ?P ?{
       ?A ?S ?D ?F ?G ?H ?J ?K ?L ?\: ?\"
       ?Z ?X ?C ?V ?B ?N ?M         ?\?))

(defconst skk-pskana-set-henkan-point-key-jis
  '(?Q ?W ?E ?R ?T ?Y ?U ?I ?O ?P ?\`
       ?A ?S ?D ?F ?G ?H ?J ?K ?L ?\+ ?\*
       ?Z ?X ?C ?V ?B ?N ?M         ?\? ?\_))

(defconst skk-pskana-set-henkan-point-key-dvorak
  '(?\" ?, ?. ?P ?Y ?F ?G ?C ?R ?L ?\/ ?\=
        ?A ?O ?E ?U ?I ?D ?H ?T ?N ?S ?-
        ?\: ?Q ?J ?K ?X ?B ?M ?W ?V ?Z))

(defconst skk-pskana-downcase-alist-us
  '((?\{ . ?\[)
    (?\} . ?\])
    (?\: . ?\;)
    (?\? . ?\/)
    (?\" . ?\')))

(defconst skk-pskana-downcase-alist-jis
  '((?\` . ?\@)
    (?\+ . ?\;)
    (?\* . ?\:)
    (?\" . ?\')
    (?\_ . ?\\)))

(defconst skk-pskana-downcase-alist-dvorak
  '((?\" . ?\')
    (?\? . ?\/)
    (?\+ . ?\=)
    (?\_ . ?\-)
    (?\: . ?\;)))

;;; Layouts
(defconst skk-pskana-K 'skk-current-kuten)
(defconst skk-pskana-T 'skk-current-touten)
(defconst skk-pskana-D 'skk-kanagaki-dakuten)
(defconst skk-pskana-H 'skk-kanagaki-handakuten)

(defconst skk-pskana-rom-kana-rule-list-hana-us
  `(
    ;; left
    ("z"  nil ("サ" . "さ")) ("a"  nil ("ス" . "す")) ("q"  nil ("ョ" . "ょ"))
    ("x"  nil ("シ" . "し")) ("s"  nil ("カ" . "か")) ("w"  nil ("テ" . "て"))
    ("c"  nil ("ナ" . "な"))                          ("e"  nil ("ト" . "と"))
    ("v"  nil ("ノ" . "の")) ("f"  nil ("キ" . "き")) ("r"  nil ("コ" . "こ"))
    ("b"  nil ("ニ" . "に")) ("g"  nil ("タ" . "た")) ("t"  nil ("ハ" . "は"))
    ;; right
    ("n"  nil ("イ" . "い")) ("h"  nil ("ン" . "ん")) ("y"  nil ("ッ" . "っ"))
    ("m"  nil ("ツ" . "つ")) ("j"  nil ("レ" . "れ")) ("u"  nil ("ク" . "く"))
    (","  nil ,skk-pskana-K)                          ("i"  nil ("ウ" . "う"))
    ("."  nil ,skk-pskana-T) ("l"  nil ,skk-pskana-D) ("o"  nil ("ル" . "る"))
    ("/"  nil ("メ" . "め")) (";"  nil ("ロ" . "ろ")) ("p"  nil ("ラ" . "ら"))
                             ("'"  nil ("リ" . "り")) ("["  nil "ー")
                                                      ("]"  nil ("エ" . "え"))
    ;; right-to-left
    ("kz" nil ("ゥ" . "ぅ")) ("ka" nil ("ァ" . "ぁ")) ("kq" nil ("ヒ" . "ひ"))
    ("kx" nil ("セ" . "せ")) ("ks" nil ("ヨ" . "よ")) ("kw" nil ("ケ" . "け"))
    ("kc" nil ("ア" . "あ")) ("kd" nil ("ュ" . "ゅ")) ("ke" nil ("ェ" . "ぇ"))
    ("kv" nil ("ワ" . "わ")) ("kf" nil ("ヤ" . "や")) ("kr" nil ("ホ" . "ほ"))
    ("kb" nil ("ユ" . "ゆ")) ("kg" nil ,skk-pskana-H) ("kt" nil ("ヘ" . "へ"))
    ;; left-to-right
    ("dn" nil ("ネ" . "ね")) ("dh" nil "・")          ("dy" nil ("ャ" . "ゃ"))
    ("dm" nil ("ミ" . "み")) ("dj" nil ("フ" . "ふ")) ("du" nil ("マ" . "ま"))
    ("d," nil ("ヲ" . "を")) ("dk" nil ("チ" . "ち")) ("di" nil ("ソ" . "そ"))
    ("d." nil ("オ" . "お")) ("dl" nil ("ム" . "む")) ("do" nil ("モ" . "も"))
    ("d/" nil ("ヌ" . "ぬ")) ("d;" nil ("ォ" . "ぉ")) ("dp" nil ("ィ" . "ぃ"))
                             ("d'" nil "」")          ("d[" nil "「")

    )
  "US 101/104 キーボードで花配列を実現するためのルール。")

(defconst skk-pskana-rom-kana-rule-list-hana-jis
  `(
    ;; left
    ("z"  nil ("サ" . "さ")) ("a"  nil ("ス" . "す")) ("q"  nil ("ョ" . "ょ"))
    ("x"  nil ("シ" . "し")) ("s"  nil ("カ" . "か")) ("w"  nil ("テ" . "て"))
    ("c"  nil ("ナ" . "な"))                          ("e"  nil ("ト" . "と"))
    ("v"  nil ("ノ" . "の")) ("f"  nil ("キ" . "き")) ("r"  nil ("コ" . "こ"))
    ("b"  nil ("ニ" . "に")) ("g"  nil ("タ" . "た")) ("t"  nil ("ハ" . "は"))
    ;; right
    ("n"  nil ("イ" . "い")) ("h"  nil ("ン" . "ん")) ("y"  nil ("ッ" . "っ"))
    ("m"  nil ("ツ" . "つ")) ("j"  nil ("レ" . "れ")) ("u"  nil ("ク" . "く"))
    (","  nil ,skk-pskana-K)                          ("i"  nil ("ウ" . "う"))
    ("."  nil ,skk-pskana-T) ("l"  nil ,skk-pskana-D) ("o"  nil ("ル" . "る"))
    ("/"  nil ("メ" . "め")) (";"  nil ("ロ" . "ろ")) ("p"  nil ("ラ" . "ら"))
    ("\\" nil ("エ" . "え")) (":"  nil ("リ" . "り")) ("@"  nil "ー")

    ;; right-to-left
    ("kz" nil ("ゥ" . "ぅ")) ("ka" nil ("ァ" . "ぁ")) ("kq" nil ("ヒ" . "ひ"))
    ("kx" nil ("セ" . "せ")) ("ks" nil ("ヨ" . "よ")) ("kw" nil ("ケ" . "け"))
    ("kc" nil ("ア" . "あ")) ("kd" nil ("ュ" . "ゅ")) ("ke" nil ("ェ" . "ぇ"))
    ("kv" nil ("ワ" . "わ")) ("kf" nil ("ヤ" . "や")) ("kr" nil ("ホ" . "ほ"))
    ("kb" nil ("ユ" . "ゆ")) ("kg" nil ,skk-pskana-H) ("kt" nil ("ヘ" . "へ"))
    ;; left-to-right
    ("dn" nil ("ネ" . "ね")) ("dh" nil "・")          ("dy" nil ("ャ" . "ゃ"))
    ("dm" nil ("ミ" . "み")) ("dj" nil ("フ" . "ふ")) ("du" nil ("マ" . "ま"))
    ("d," nil ("ヲ" . "を")) ("dk" nil ("チ" . "ち")) ("di" nil ("ソ" . "そ"))
    ("d." nil ("オ" . "お")) ("dl" nil ("ム" . "む")) ("do" nil ("モ" . "も"))
    ("d/" nil ("ヌ" . "ぬ")) ("d;" nil ("ォ" . "ぉ")) ("dp" nil ("ィ" . "ぃ"))
                             ("d:" nil "」")          ("d@" nil "「")
    )
  "JIS X 6002-1980 キーボードで花配列を実現するためのルール。")

(defconst skk-pskana-rom-kana-rule-list-hana-dvorak
  `(
    ;; left
    (";"  nil ("サ" . "さ")) ("a"  nil ("ス" . "す")) ("'"  nil ("ョ" . "ょ"))
    ("q"  nil ("シ" . "し")) ("o"  nil ("カ" . "か")) (","  nil ("テ" . "て"))
    ("j"  nil ("ナ" . "な"))                          ("."  nil ("ト" . "と"))
    ("k"  nil ("ノ" . "の")) ("i"  nil ("キ" . "き")) ("p"  nil ("コ" . "こ"))
    ("x"  nil ("ニ" . "に")) ("u"  nil ("タ" . "た")) ("y"  nil ("ハ" . "は"))
    ;; right
    ("b"  nil ("イ" . "い")) ("d"  nil ("ン" . "ん")) ("f"  nil ("ッ" . "っ"))
    ("m"  nil ("ツ" . "つ")) ("h"  nil ("レ" . "れ")) ("g"  nil ("ク" . "く"))
    ("w"  nil ,skk-pskana-K)                          ("c"  nil ("ウ" . "う"))
    ("v"  nil ,skk-pskana-T) ("n"  nil ,skk-pskana-D) ("r"  nil ("ル" . "る"))
    ("z"  nil ("メ" . "め")) ("s"  nil ("ロ" . "ろ")) ("l"  nil ("ラ" . "ら"))
                             ("-"  nil ("リ" . "り")) ("/"  nil "ー")
                                                      ("="  nil ("エ" . "え"))
    ;; right-to-left
    ("t;" nil ("ゥ" . "ぅ")) ("ta" nil ("ァ" . "ぁ")) ("t'" nil ("ヒ" . "ひ"))
    ("tq" nil ("セ" . "せ")) ("to" nil ("ヨ" . "よ")) ("t," nil ("ケ" . "け"))
    ("tj" nil ("ア" . "あ")) ("te" nil ("ュ" . "ゅ")) ("t." nil ("ェ" . "ぇ"))
    ("tk" nil ("ワ" . "わ")) ("ti" nil ("ヤ" . "や")) ("tp" nil ("ホ" . "ほ"))
    ("tx" nil ("ユ" . "ゆ")) ("tu" nil ,skk-pskana-H) ("ty" nil ("ヘ" . "へ"))
    ;; left-to-right
    ("eb" nil ("ネ" . "ね")) ("eh" nil "・")          ("ef" nil ("ャ" . "ゃ"))
    ("em" nil ("ミ" . "み")) ("eh" nil ("フ" . "ふ")) ("eg" nil ("マ" . "ま"))
    ("ew" nil ("ヲ" . "を")) ("et" nil ("チ" . "ち")) ("ec" nil ("ソ" . "そ"))
    ("ev" nil ("オ" . "お")) ("en" nil ("ム" . "む")) ("er" nil ("モ" . "も"))
    ("ez" nil ("ヌ" . "ぬ")) ("es" nil ("ォ" . "ぉ")) ("el" nil ("ィ" . "ぃ"))
                             ("e-" nil "」")          ("e/" nil "「")
    )
  "Dvorak キーボードで花配列を実現するためのルール。")

(defconst skk-pskana-rom-kana-rule-list-tsuki-2-263-us
  `(
    ;; left
    ("z"  nil ("ス" . "す")) ("a"  nil ("ハ" . "は")) ("q"  nil ("ソ" . "そ"))
    ("x"  nil ("ケ" . "け")) ("s"  nil ("カ" . "か")) ("w"  nil ("コ" . "こ"))
    ("c"  nil ("ニ" . "に"))                          ("e"  nil ("シ" . "し"))
    ("v"  nil ("ナ" . "な")) ("f"  nil ("ト" . "と")) ("r"  nil ("テ" . "て"))
    ("b"  nil ("サ" . "さ")) ("g"  nil ("タ" . "た")) ("t"  nil ("ョ" . "ょ"))
    ;; right
    ("n"  nil ("ッ" . "っ")) ("h"  nil ("ク" . "く")) ("y"  nil ("ツ" . "つ"))
    ("m"  nil ("ル" . "る")) ("j"  nil ("ウ" . "う")) ("u"  nil ("ン" . "ん"))
    (","  nil ,skk-pskana-T)                          ("i"  nil ("イ" . "い"))
    ("."  nil ,skk-pskana-K) ("l"  nil ,skk-pskana-D) ("o"  nil ("ノ" . "の"))
    ("/"  nil ,skk-pskana-H) (";"  nil ("キ" . "き")) ("p"  nil ("リ" . "り"))
                             ("'"  nil ("レ" . "れ")) ("["  nil ("チ" . "ち"))
                                                      ("]"  nil "・")
    ;; right-to-left
    ("kz" nil ("ゥ" . "ぅ")) ("ka" nil ("ィ" . "ぃ")) ("kq" nil ("ァ" . "ぁ"))
    ("kx" nil ("ヘ" . "へ")) ("ks" nil ("ヲ" . "を")) ("kw" nil ("ヒ" . "ひ"))
    ("kc" nil ("セ" . "せ")) ("kd" nil ("ラ" . "ら")) ("ke" nil ("ホ" . "ほ"))
    ("kv" nil ("ュ" . "ゅ")) ("kf" nil ("ア" . "あ")) ("kr" nil ("フ" . "ふ"))
    ("kb" nil ("ャ" . "ゃ")) ("kg" nil ("ヨ" . "よ")) ("kt" nil ("メ" . "め"))
    ;; left-to-right
    ("dn" nil ("ム" . "む")) ("dh" nil ("マ" . "ま")) ("dy" nil ("ヌ" . "ぬ"))
    ("dm" nil ("ロ" . "ろ")) ("dj" nil ("オ" . "お")) ("du" nil ("エ" . "え"))
    ("d," nil ("ネ" . "ね")) ("dk" nil ("モ" . "も")) ("di" nil ("ミ" . "み"))
    ("d." nil "ー")          ("dl" nil ("ワ" . "わ")) ("do" nil ("ヤ" . "や"))
    ("d/" nil ("ォ" . "ぉ")) ("d;" nil ("ユ" . "ゆ")) ("dp" nil ("ェ" . "ぇ"))
                             ("d'" nil "」")          ("d[" nil "「")
    )
  "US 101/104 キーボードで月配列 2-263 を実現するためのルール。")

(defconst skk-pskana-rom-kana-rule-list-tsuki-2-263-jis
  `(
    ;; left
    ("z"  nil ("ス" . "す")) ("a"  nil ("ハ" . "は")) ("q"  nil ("ソ" . "そ"))
    ("x"  nil ("ケ" . "け")) ("s"  nil ("カ" . "か")) ("w"  nil ("コ" . "こ"))
    ("c"  nil ("ニ" . "に"))                          ("e"  nil ("シ" . "し"))
    ("v"  nil ("ナ" . "な")) ("f"  nil ("ト" . "と")) ("r"  nil ("テ" . "て"))
    ("b"  nil ("サ" . "さ")) ("g"  nil ("タ" . "た")) ("t"  nil ("ョ" . "ょ"))
    ;; right
    ("n"  nil ("ッ" . "っ")) ("h"  nil ("ク" . "く")) ("y"  nil ("ツ" . "つ"))
    ("m"  nil ("ル" . "る")) ("j"  nil ("ウ" . "う")) ("u"  nil ("ン" . "ん"))
    (","  nil ,skk-pskana-T)                          ("i"  nil ("イ" . "い"))
    ("."  nil ,skk-pskana-K) ("l"  nil ,skk-pskana-D) ("o"  nil ("ノ" . "の"))
    ("/"  nil ,skk-pskana-H) (";"  nil ("キ" . "き")) ("p"  nil ("リ" . "り"))
    ("\\" nil "・")          (":"  nil ("レ" . "れ")) ("@"  nil ("チ" . "ち"))

    ;; right-to-left
    ("kz" nil ("ゥ" . "ぅ")) ("ka" nil ("ィ" . "ぃ")) ("kq" nil ("ァ" . "ぁ"))
    ("kx" nil ("ヘ" . "へ")) ("ks" nil ("ヲ" . "を")) ("kw" nil ("ヒ" . "ひ"))
    ("kc" nil ("セ" . "せ")) ("kd" nil ("ラ" . "ら")) ("ke" nil ("ホ" . "ほ"))
    ("kv" nil ("ュ" . "ゅ")) ("kf" nil ("ア" . "あ")) ("kr" nil ("フ" . "ふ"))
    ("kb" nil ("ャ" . "ゃ")) ("kg" nil ("ヨ" . "よ")) ("kt" nil ("メ" . "め"))
    ;; left-to-right
    ("dn" nil ("ム" . "む")) ("dh" nil ("マ" . "ま")) ("dy" nil ("ヌ" . "ぬ"))
    ("dm" nil ("ロ" . "ろ")) ("dj" nil ("オ" . "お")) ("du" nil ("エ" . "え"))
    ("d," nil ("ネ" . "ね")) ("dk" nil ("モ" . "も")) ("di" nil ("ミ" . "み"))
    ("d." nil "ー")          ("dl" nil ("ワ" . "わ")) ("do" nil ("ヤ" . "や"))
    ("d/" nil ("ォ" . "ぉ")) ("d;" nil ("ユ" . "ゆ")) ("dp" nil ("ェ" . "ぇ"))
                             ("d:" nil "」")          ("d@" nil "「")
    )
  "JIS X 6002-1980 キーボードで月配列 2-263 を実現するためのルール。")

(defconst skk-pskana-rom-kana-rule-list-tsuki-2-263-dvorak
  `(
    ;; left
    (";"  nil ("ス" . "す")) ("a"  nil ("ハ" . "は")) ("'"  nil ("ソ" . "そ"))
    ("q"  nil ("ケ" . "け")) ("o"  nil ("カ" . "か")) (","  nil ("コ" . "こ"))
    ("j"  nil ("ニ" . "に"))                          ("."  nil ("シ" . "し"))
    ("k"  nil ("ナ" . "な")) ("u"  nil ("ト" . "と")) ("p"  nil ("テ" . "て"))
    ("x"  nil ("サ" . "さ")) ("i"  nil ("タ" . "た")) ("y"  nil ("ョ" . "ょ"))
    ;; right
    ("b"  nil ("ッ" . "っ")) ("d"  nil ("ク" . "く")) ("f"  nil ("ツ" . "つ"))
    ("m"  nil ("ル" . "る")) ("h"  nil ("ウ" . "う")) ("g"  nil ("ン" . "ん"))
    ("w"  nil ,skk-pskana-T)                          ("c"  nil ("イ" . "い"))
    ("v"  nil ,skk-pskana-K) ("n"  nil ,skk-pskana-D) ("r"  nil ("ノ" . "の"))
    ("z"  nil ,skk-pskana-H) ("s"  nil ("キ" . "き")) ("l"  nil ("リ" . "り"))
                             ("-"  nil ("レ" . "れ")) ("/"  nil ("チ" . "ち"))
                                                      ("="  nil "・")
    ;; right-to-left
    ("t;" nil ("ゥ" . "ぅ")) ("ta" nil ("ィ" . "ぃ")) ("t'" nil ("ァ" . "ぁ"))
    ("tq" nil ("ヘ" . "へ")) ("to" nil ("ヲ" . "を")) ("t," nil ("ヒ" . "ひ"))
    ("tj" nil ("セ" . "せ")) ("te" nil ("ラ" . "ら")) ("t." nil ("ホ" . "ほ"))
    ("tk" nil ("ュ" . "ゅ")) ("tu" nil ("ア" . "あ")) ("tp" nil ("フ" . "ふ"))
    ("tx" nil ("ャ" . "ゃ")) ("ti" nil ("ヨ" . "よ")) ("ty" nil ("メ" . "め"))
    ;; left-to-right
    ("eb" nil ("ム" . "む")) ("ed" nil ("マ" . "ま")) ("ef" nil ("ヌ" . "ぬ"))
    ("em" nil ("ロ" . "ろ")) ("eh" nil ("オ" . "お")) ("eg" nil ("エ" . "え"))
    ("ew" nil ("ネ" . "ね")) ("et" nil ("モ" . "も")) ("ec" nil ("ミ" . "み"))
    ("ev" nil "ー")          ("en" nil ("ワ" . "わ")) ("er" nil ("ヤ" . "や"))
    ("ez" nil ("ォ" . "ぉ")) ("es" nil ("ユ" . "ゆ")) ("el" nil ("ェ" . "ぇ"))
                             ("e-" nil "」")          ("e/" nil "「")
    )
  "Dvorak キーボードで月配列 2-263 を実現するためのルール。")

(defadvice skk-set-henkan-point (around
                                 skk-pskana-dakuten-workaround
                                 activate compile)
  "\
送りがなの開始文字が濁点・半濁点のとき、可能ならば
直前の文字にそれを付したものを送りがなの開始文字にする。"
  (or (-when-let* ((_ skk-henkan-mode)
                   (c (skk-downcase last-command-event))
                   (next (skk-select-branch
                          (or skk-current-rule-tree skk-rule-tree) c))
                   (_ (null (skk-get-branch-list next)))
                   (s (skk-get-kana next))
                   (_ (memq s '(skk-kanagaki-dakuten
                                skk-kanagaki-handakuten))))
        (funcall s)
        (skk-set-char-before-as-okurigana)
        t)
      ad-do-it))

(defsubst skk-pskana--val (str &rest objs)
  (symbol-value (intern (apply #'format str objs))))

(defun skk-pskana-init ()
  (setq skk-downcase-alist
        (skk-pskana--val "skk-pskana-downcase-alist-%s"
                         skk-pskana-keyboard-layout))
  (setq skk-set-henkan-point-key
        (skk-pskana--val "skk-pskana-set-henkan-point-key-%s"
                         skk-pskana-keyboard-layout))
  (setq skk-previous-candidate-keys
        (skk-pskana--val "skk-pskana-previous-candidate-keys-%s"
                         skk-pskana-keyboard-layout))
  (setq skk-rom-kana-base-rule-list
        (append
         (skk-pskana--val "skk-pskana-rom-kana-base-rule-list-%s-%s"
                          skk-pskana-kana-layout
                          skk-pskana-keyboard-layout)
         (skk-pskana--val "skk-pskana-rom-kana-rule-list-%s-%s"
                          skk-pskana-kana-layout
                          skk-pskana-keyboard-layout)))
  )

(defadvice skk-restart (after skk-pskana-restart activate)
  "skk-restart時にskk-pskanaも再設定されるようにする"
  (skk-pskana-init)
  (setq skk-rule-tree (skk-compile-rule-list skk-rom-kana-base-rule-list
                                             skk-rom-kana-rule-list)))

(skk-pskana-init)

(defun skk-pskana-make-hankaku-rule (rule-list)
  (cl-loop for (in next out) in rule-list
           collect (list in
                         next
                         (cond ((consp out)
                                (japanese-hankaku (car out)))
                               ((stringp out)
                                (japanese-hankaku out))
                               (t
                                out)))))

(defun skk-pskana-jisx0201-init ()
  (setq skk-jisx0201-base-rule-list
        (skk-pskana-make-hankaku-rule skk-rom-kana-base-rule-list))
  (setq skk-jisx0201-rule-list
        (skk-pskana-make-hankaku-rule skk-rom-kana-rule-list)))

(defun skk-pskana-jisx0201-compile-tree ()
  (setq skk-jisx0201-base-rule-tree
        (skk-compile-rule-list skk-jisx0201-base-rule-list
                               skk-jisx0201-rule-list)))

(eval-after-load "skk-jisx0201"
  '(progn
     (defadvice skk-pskana-init (after skk-pskana-jisx0201-init last activate)
       (skk-pskana-jisx0201-init))
     (defadvice skk-restart (after skk-pskana-jisx0201-restart last activate)
       (skk-pskana-jisx0201-compile-tree))
     (skk-pskana-jisx0201-init)
     (skk-pskana-jisx0201-compile-tree)
     ))

(provide 'skk-pskana)
;;; skk-pskana.el ends here
