;;; skk-pskana.el --- Prefix Shift Kana Input Method for SKK
;;
;; -*- coding: euc-jp -*-
;;
;; Copyright (C) 2006-2013 OOHASHI, Daichi <leque@katch.ne.jp>
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;;
;; Author: OOHASHI, Daichi <leque@katch.ne.jp>
;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;;         of ddskk/nicola/skk-kanagaki.el
;;
;; This file is *NOT* part of Daredevil SKK.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; long with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;
;; * What is this?
;;
;; This is an utility to input Japanese with prefix shift kana layout,
;; such as
;; :������ (Hana layout):
;;   http://homepage3.nifty.com/togasi/hana_no_kuni/
;; :������ 2-263 (Tsuki-2-263 layout):
;;   http://jisx6004.client.jp/tsuki.html
;; :Some variants of Tsuki:
;;   http://yellow.ribbon.to/%7Eujiro/hairetu.htm
;; and the like.
;;
;; Currently provided layout definitions are those of Hana layout
;; and Tsuki-2-263 layout for US/JIS/Dvorak keyboard.
;;
;;
;; * Requirements:
;;
;; - Daredevil SKK: http://openlab.jp/skk/main-ja.html
;; - skk-kanagaki-util.el (in SKK package)
;;
;;
;; * How to use
;;
;; Install Daredevil SKK and skk-kanagaki-util.el, and copy this file
;; to a dicrectory in *load-path*.
;; 
;; To use this, put your .skk below:
;;   (require 'skk-kanagaki-util)
;;   (load "skk-pskana")
;;
;; The default layout is Tsuki-2-263 on US keyboard.
;; To use Hana layout on Dvorak keyboard, for example, put your .skk
;;   (setq skk-pskana-keyboard-type 'dvorak)
;;   (setq skk-pskana-keyboard-layout 'hana)
;; before loading "skk-pskana", or do `M-x skk-pskana-init' after setting.
;;
;; You could not have difficulty to use (at least once you have learned
;; the layout, and are familiar to SKK), but there is a little bit
;; difference from normal SKK to input an okurigana with dakuten/handakuten.
;; 
;; On normal SKK, for instance, to input `ͷ��', you have simply to type
;; `AsoBu'. But here, you have to type ������<Shift>��: type a okurigana
;; character without dakuten/handakuten, and hold down a Shift key and
;; type dakuten/handakuten.
;; 
;; I.e., with tsuki-2-263, for `ͷ��', you have to type `KfqkrL':
;; `Kf' for `��', `q' for `��', `kr' for `��', and `L' for dakuten.
;; For `���٤�', type `GkxLm': `G' for `��', `kx' for `��', `L' for dakuten,
;; and `m' for `��'.
;;
;;
;; * Note
;;
;; I use only tsuki-2-263-us, the others are not tested at all.
;; Use them at your own risk.
;; The source is always with you.
;;

(eval-when-compile
  (require 'skk)
  (require 'skk-kanagaki-util)
  (require 'skk-vars)
  )

(defgroup skk-pskana nil "SKK prefix shift kana input related customization."
  :prefix "skk-pskana-"
  :group 'skk-custom-by-filename)

(defcustom skk-pskana-keyboard-type 'us
  "*���Ѥ��륭���ܡ��ɤμ���"
  :type '(choice (const us)
		 (const jis)
		 (const dvorak)
		 (symbol :tag "Another Keyboard Type"))
  :group 'skk-pskana)

(defcustom skk-pskana-keyboard-layout 'tsuki-2-263
  "*���Ѥ��뤫������μ���"
  :type '(choice (const tsuki-2-263)
		 (const hana)
		 (symbol :tag "Another Keyboard Layout"))
  :group 'skk-pskana)


;;; Function Keys
(defconst skk-pskana-rom-kana-base-rule-list-hana-us
  '(("dq" nil skk-toggle-kana)
    ("kl" nil skk-latin-mode)
    ("ko" nil skk-jisx0208-latin-mode)
    ("dx" nil skk-purge-from-jisyo)
    ("k/" nil skk-abbrev-mode)
    ("da" nil skk-set-henkan-point-subr)
    ("\\" nil skk-input-by-code-or-menu)
    ("$" nil skk-display-code-for-char-at-point)
    ("&" nil skk-today)
    (skk-kakutei-key nil skk-kakutei)
    ))

(defconst skk-pskana-rom-kana-base-rule-list-tsuki-2-263-us
  skk-pskana-rom-kana-base-rule-list-hana-us)

(defconst skk-pskana-rom-kana-base-rule-list-hana-jis
  skk-pskana-rom-kana-base-rule-list-hana-us)

(defconst skk-pskana-rom-kana-base-rule-list-hana-jis
  skk-pskana-rom-kana-base-rule-list-tsuki-2-263-us)

(defconst skk-pskana-rom-kana-base-rule-list-hana-dvorak
  '(("eq" nil skk-toggle-kana)
    ("tl" nil skk-latin-mode)
    ("tr" nil skk-jisx0208-latin-mode)
    ("ex" nil skk-purge-from-jisyo)
    ("t/" nil skk-abbrev-mode)
    ("eo" nil skk-set-henkan-point-subr)
    ("#" nil skk-input-by-code-or-menu)
    ("$" nil skk-display-code-for-char-at-point)
    ("&" nil skk-today)
    (skk-kakutei-key nil skk-kakutei)
    ))

(defconst skk-pskana-rom-kana-base-rule-list-tsuki-2-263-dvorak
  skk-pskana-rom-kana-base-rule-list-hana-dvorak)


;;; Use dakuten key for previous candidate
(defconst skk-pskana-previous-candidate-char-us ?-)

(defconst skk-pskana-previous-candidate-char-jis ?-)

(defconst skk-pskana-previous-candidate-char-dvorak ?\[)


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

(defconst skk-pskana-rom-kana-rule-list-hana-us
  '(("z" nil ("��" . "��"))  ("a" nil ("��" . "��"))  ("q" nil ("��" . "��"))
    ("x" nil ("��" . "��"))  ("s" nil ("��" . "��"))  ("w" nil ("��" . "��"))
    ("c" nil ("��" . "��"))                           ("e" nil ("��" . "��"))
    ("v" nil ("��" . "��"))  ("f" nil ("��" . "��"))  ("r" nil ("��" . "��"))
    ("b" nil ("��" . "��"))  ("g" nil ("��" . "��"))  ("t" nil ("��" . "��"))
    ("n" nil ("��" . "��"))  ("h" nil ("��" . "��"))  ("y" nil ("��" . "��"))
    ("m" nil ("��" . "��"))  ("j" nil ("��" . "��"))  ("u" nil ("��" . "��"))
    ("," nil skk-current-kuten)                       ("i" nil ("��" . "��"))
    ("." nil skk-current-touten) ("l" nil skk-kanagaki-dakuten) ("o" nil ("��" . "��"))
    ("/" nil ("��" . "��"))  (";" nil ("��" . "��"))  ("p" nil ("��" . "��"))
                             ("'" nil ("��" . "��"))  ("[" nil "��")
                                                      ("]" nil ("��" . "��"))
    ;; prefix-key: k
    ("kz" nil ("��" . "��")) ("ka" nil ("��" . "��")) ("kq" nil ("��" . "��"))
    ("kx" nil ("��" . "��")) ("ks" nil ("��" . "��")) ("kw" nil ("��" . "��"))
    ("kc" nil ("��" . "��")) ("kd" nil ("��" . "��")) ("ke" nil ("��" . "��"))
    ("kv" nil ("��" . "��")) ("kf" nil ("��" . "��")) ("kr" nil ("��" . "��"))
    ("kb" nil ("��" . "��")) ("kg" nil skk-kanagaki-handakuten) ("kt" nil ("��" . "��"))
    ;; prefix-key: d
    ("dn" nil ("��" . "��")) ("dh" nil "��")          ("dy" nil ("��" . "��"))
    ("dm" nil ("��" . "��")) ("dj" nil ("��" . "��")) ("du" nil ("��" . "��"))
    ("d," nil ("��" . "��")) ("dk" nil ("��" . "��")) ("di" nil ("��" . "��"))
    ("d." nil ("��" . "��")) ("dl" nil ("��" . "��")) ("do" nil ("��" . "��"))
    ("d/" nil ("��" . "��")) ("d;" nil ("��" . "��")) ("dp" nil ("��" . "��"))
                             ("d'" nil "��")          ("d[" nil "��")

    )
  "US 101/104 �����ܡ��ɤǲ������¸����뤿��Υ롼�롣")

(defconst skk-pskana-rom-kana-rule-list-hana-jis
  '(("z" nil ("��" . "��"))  ("a" nil ("��" . "��"))  ("q" nil ("��" . "��"))
    ("x" nil ("��" . "��"))  ("s" nil ("��" . "��"))  ("w" nil ("��" . "��"))
    ("c" nil ("��" . "��"))                           ("e" nil ("��" . "��"))
    ("v" nil ("��" . "��"))  ("f" nil ("��" . "��"))  ("r" nil ("��" . "��"))
    ("b" nil ("��" . "��"))  ("g" nil ("��" . "��"))  ("t" nil ("��" . "��"))
    ("n" nil ("��" . "��"))  ("h" nil ("��" . "��"))  ("y" nil ("��" . "��"))
    ("m" nil ("��" . "��"))  ("j" nil ("��" . "��"))  ("u" nil ("��" . "��"))
    ("," nil skk-current-kuten)                       ("i" nil ("��" . "��"))
    ("." nil skk-current-touten) ("l" nil skk-kanagaki-dakuten) ("o" nil ("��" . "��"))
    ("/" nil ("��" . "��"))  (";" nil ("��" . "��"))  ("p" nil ("��" . "��"))
    ("\\" nil ("��" . "��")) (":" nil ("��" . "��"))  ("@" nil "��")

    ;; prefix-key: k
    ("kz" nil ("��" . "��")) ("ka" nil ("��" . "��")) ("kq" nil ("��" . "��"))
    ("kx" nil ("��" . "��")) ("ks" nil ("��" . "��")) ("kw" nil ("��" . "��"))
    ("kc" nil ("��" . "��")) ("kd" nil ("��" . "��")) ("ke" nil ("��" . "��"))
    ("kv" nil ("��" . "��")) ("kf" nil ("��" . "��")) ("kr" nil ("��" . "��"))
    ("kb" nil ("��" . "��")) ("kg" nil skk-kanagaki-handakuten) ("kt" nil ("��" . "��"))
    ;; prefix-key: d
    ("dn" nil ("��" . "��")) ("dh" nil "��")          ("dy" nil ("��" . "��"))
    ("dm" nil ("��" . "��")) ("dj" nil ("��" . "��")) ("du" nil ("��" . "��"))
    ("d," nil ("��" . "��")) ("dk" nil ("��" . "��")) ("di" nil ("��" . "��"))
    ("d." nil ("��" . "��")) ("dl" nil ("��" . "��")) ("do" nil ("��" . "��"))
    ("d/" nil ("��" . "��")) ("d;" nil ("��" . "��")) ("dp" nil ("��" . "��"))
                             ("d:" nil "��")          ("d@" nil "��")
    )
  "JIS X 6002-1980 �����ܡ��ɤǲ������¸����뤿��Υ롼�롣")

(defconst skk-pskana-rom-kana-rule-list-hana-dvorak
  '((";" nil ("��" . "��"))  ("a" nil ("��" . "��"))  ("'" nil ("��" . "��"))
    ("q" nil ("��" . "��"))  ("o" nil ("��" . "��"))  ("," nil ("��" . "��"))
    ("j" nil ("��" . "��"))                           ("." nil ("��" . "��"))
    ("k" nil ("��" . "��"))  ("i" nil ("��" . "��"))  ("p" nil ("��" . "��"))
    ("x" nil ("��" . "��"))  ("u" nil ("��" . "��"))  ("y" nil ("��" . "��"))
    ("b" nil ("��" . "��"))  ("d" nil ("��" . "��"))  ("f" nil ("��" . "��"))
    ("m" nil ("��" . "��"))  ("h" nil ("��" . "��"))  ("g" nil ("��" . "��"))
    ("w" nil skk-current-kuten)                       ("c" nil ("��" . "��"))
    ("v" nil skk-current-touten) ("n" nil skk-kanagaki-dakuten) ("r" nil ("��" . "��"))
    ("z" nil ("��" . "��"))  ("s" nil ("��" . "��"))  ("l" nil ("��" . "��"))
                             ("-" nil ("��" . "��"))  ("/" nil "��")
                                                      ("=" nil ("��" . "��"))
    ;; prefix-key: k
    ("t;" nil ("��" . "��")) ("ta" nil ("��" . "��")) ("t'" nil ("��" . "��"))
    ("tq" nil ("��" . "��")) ("to" nil ("��" . "��")) ("t," nil ("��" . "��"))
    ("tj" nil ("��" . "��")) ("te" nil ("��" . "��")) ("t." nil ("��" . "��"))
    ("tk" nil ("��" . "��")) ("ti" nil ("��" . "��")) ("tp" nil ("��" . "��"))
    ("tx" nil ("��" . "��")) ("tu" nil skk-kanagaki-handakuten) ("ty" nil ("��" . "��"))
    ;; prefix-key: d
    ("eb" nil ("��" . "��")) ("eh" nil "��")          ("ef" nil ("��" . "��"))
    ("em" nil ("��" . "��")) ("eh" nil ("��" . "��")) ("eg" nil ("��" . "��"))
    ("ew" nil ("��" . "��")) ("et" nil ("��" . "��")) ("ec" nil ("��" . "��"))
    ("ev" nil ("��" . "��")) ("en" nil ("��" . "��")) ("er" nil ("��" . "��"))
    ("ez" nil ("��" . "��")) ("es" nil ("��" . "��")) ("el" nil ("��" . "��"))
                             ("e-" nil "��")          ("e/" nil "��")
    )
  "Dvorak �����ܡ��ɤǲ������¸����뤿��Υ롼�롣")


(defconst skk-pskana-rom-kana-rule-list-tsuki-2-263-us
  '(("z" nil ("��" . "��"))  ("a" nil ("��" . "��"))  ("q" nil ("��" . "��"))
    ("x" nil ("��" . "��"))  ("s" nil ("��" . "��"))  ("w" nil ("��" . "��"))
    ("c" nil ("��" . "��"))                           ("e" nil ("��" . "��"))
    ("v" nil ("��" . "��"))  ("f" nil ("��" . "��"))  ("r" nil ("��" . "��"))
    ("b" nil ("��" . "��"))  ("g" nil ("��" . "��"))  ("t" nil ("��" . "��"))
    ("n" nil ("��" . "��"))  ("h" nil ("��" . "��"))  ("y" nil ("��" . "��"))
    ("m" nil ("��" . "��"))  ("j" nil ("��" . "��"))  ("u" nil ("��" . "��"))
    ("," nil skk-current-touten)                      ("i" nil ("��" . "��"))
    ("." nil skk-current-kuten) ("l" nil skk-kanagaki-dakuten) ("o" nil ("��" . "��"))
    ("/" nil skk-kanagaki-handakuten) (";" nil ("��" . "��")) ("p" nil ("��" . "��"))
                             ("'" nil ("��" . "��"))  ("[" nil ("��" . "��"))
                                                      ("]" nil "��")
    ;; prefix-key: k
    ("kz" nil ("��" . "��")) ("ka" nil ("��" . "��")) ("kq" nil ("��" . "��"))
    ("kx" nil ("��" . "��")) ("ks" nil ("��" . "��")) ("kw" nil ("��" . "��"))
    ("kc" nil ("��" . "��")) ("kd" nil ("��" . "��")) ("ke" nil ("��" . "��"))
    ("kv" nil ("��" . "��")) ("kf" nil ("��" . "��")) ("kr" nil ("��" . "��"))
    ("kb" nil ("��" . "��")) ("kg" nil ("��" . "��")) ("kt" nil ("��" . "��"))
    ;; prefix-key: d
    ("dn" nil ("��" . "��")) ("dh" nil ("��" . "��")) ("dy" nil ("��" . "��"))
    ("dm" nil ("��" . "��")) ("dj" nil ("��" . "��")) ("du" nil ("��" . "��"))
    ("d," nil ("��" . "��")) ("dk" nil ("��" . "��")) ("di" nil ("��" . "��"))
    ("d." nil "��")          ("dl" nil ("��" . "��")) ("do" nil ("��" . "��"))
    ("d/" nil ("��" . "��")) ("d;" nil ("��" . "��")) ("dp" nil ("��" . "��"))
                             ("d'" nil "��")          ("d[" nil "��")
    )
  "US 101/104 �����ܡ��ɤǷ����� 2-263 ��¸����뤿��Υ롼�롣")


(defconst skk-pskana-rom-kana-rule-list-tsuki-2-263-jis
  '(("z" nil ("��" . "��"))  ("a" nil ("��" . "��"))  ("q" nil ("��" . "��"))
    ("x" nil ("��" . "��"))  ("s" nil ("��" . "��"))  ("w" nil ("��" . "��"))
    ("c" nil ("��" . "��"))                           ("e" nil ("��" . "��"))
    ("v" nil ("��" . "��"))  ("f" nil ("��" . "��"))  ("r" nil ("��" . "��"))
    ("b" nil ("��" . "��"))  ("g" nil ("��" . "��"))  ("t" nil ("��" . "��"))
    ("n" nil ("��" . "��"))  ("h" nil ("��" . "��"))  ("y" nil ("��" . "��"))
    ("m" nil ("��" . "��"))  ("j" nil ("��" . "��"))  ("u" nil ("��" . "��"))
    ("," nil skk-current-touten)                      ("i" nil ("��" . "��"))
    ("." nil skk-current-kuten) ("l" nil skk-kanagaki-dakuten) ("o" nil ("��" . "��"))
    ("/" nil skk-kanagaki-handakuten) (";" nil ("��" . "��")) ("p" nil ("��" . "��"))
    ("\\" nil "��")          (":" nil ("��" . "��"))  ("@" nil ("��" . "��"))

    ;; prefix-key: k
    ("kz" nil ("��" . "��")) ("ka" nil ("��" . "��")) ("kq" nil ("��" . "��"))
    ("kx" nil ("��" . "��")) ("ks" nil ("��" . "��")) ("kw" nil ("��" . "��"))
    ("kc" nil ("��" . "��")) ("kd" nil ("��" . "��")) ("ke" nil ("��" . "��"))
    ("kv" nil ("��" . "��")) ("kf" nil ("��" . "��")) ("kr" nil ("��" . "��"))
    ("kb" nil ("��" . "��")) ("kg" nil ("��" . "��")) ("kt" nil ("��" . "��"))
    ;; prefix-key: d
    ("dn" nil ("��" . "��")) ("dh" nil ("��" . "��")) ("dy" nil ("��" . "��"))
    ("dm" nil ("��" . "��")) ("dj" nil ("��" . "��")) ("du" nil ("��" . "��"))
    ("d," nil ("��" . "��")) ("dk" nil ("��" . "��")) ("di" nil ("��" . "��"))
    ("d." nil "��")          ("dl" nil ("��" . "��")) ("do" nil ("��" . "��"))
    ("d/" nil ("��" . "��")) ("d;" nil ("��" . "��")) ("dp" nil ("��" . "��"))
                             ("d:" nil "��")          ("d@" nil "��")
    )
  "JIS X 6002-1980 �����ܡ��ɤǷ����� 2-263 ��¸����뤿��Υ롼�롣")

(defconst skk-pskana-rom-kana-rule-list-tsuki-2-263-dvorak
  '((";" nil ("��" . "��"))  ("a" nil ("��" . "��"))  ("'" nil ("��" . "��"))
    ("q" nil ("��" . "��"))  ("o" nil ("��" . "��"))  ("," nil ("��" . "��"))
    ("j" nil ("��" . "��"))                           ("." nil ("��" . "��"))
    ("k" nil ("��" . "��"))  ("u" nil ("��" . "��"))  ("p" nil ("��" . "��"))
    ("x" nil ("��" . "��"))  ("i" nil ("��" . "��"))  ("y" nil ("��" . "��"))
    ("b" nil ("��" . "��"))  ("d" nil ("��" . "��"))  ("f" nil ("��" . "��"))
    ("m" nil ("��" . "��"))  ("h" nil ("��" . "��"))  ("g" nil ("��" . "��"))
    ("w" nil skk-current-touten)                      ("c" nil ("��" . "��"))
    ("v" nil skk-current-kuten) ("n" nil skk-kanagaki-dakuten) ("r" nil ("��" . "��"))
    ("z" nil skk-kanagaki-handakuten) ("s" nil ("��" . "��")) ("l" nil ("��" . "��"))
                             ("-" nil ("��" . "��"))  ("/" nil ("��" . "��"))
                                                      ("=" nil "��")
    ;; prefix-key: k
    ("t;" nil ("��" . "��")) ("ta" nil ("��" . "��")) ("t'" nil ("��" . "��"))
    ("tq" nil ("��" . "��")) ("to" nil ("��" . "��")) ("t," nil ("��" . "��"))
    ("tj" nil ("��" . "��")) ("te" nil ("��" . "��")) ("t." nil ("��" . "��"))
    ("tk" nil ("��" . "��")) ("tu" nil ("��" . "��")) ("tp" nil ("��" . "��"))
    ("tx" nil ("��" . "��")) ("ti" nil ("��" . "��")) ("ty" nil ("��" . "��"))
    ;; prefix-key: d
    ("eb" nil ("��" . "��")) ("ed" nil ("��" . "��")) ("ef" nil ("��" . "��"))
    ("em" nil ("��" . "��")) ("eh" nil ("��" . "��")) ("eg" nil ("��" . "��"))
    ("ew" nil ("��" . "��")) ("et" nil ("��" . "��")) ("ec" nil ("��" . "��"))
    ("ev" nil "��")          ("en" nil ("��" . "��")) ("er" nil ("��" . "��"))
    ("ez" nil ("��" . "��")) ("es" nil ("��" . "��")) ("el" nil ("��" . "��"))
                             ("e-" nil "��")          ("e/" nil "��")
    )
  "Dvorak �����ܡ��ɤǷ����� 2-263 ��¸����뤿��Υ롼�롣")

(put 'and-let* 'lisp-indent-function 1)
(defmacro and-let* (clause &rest body)
  "See SRFI-2: http://srfi.schemers.org/srfi-2/"
  (if (null clause)
      `(progn ,@body)
    (let ((cl (car clause)))
      (cond ((null (cdr cl))
	     `(and ,(car cl)
		   (and-let* ,(cdr clause)
			     ,@body)))
	    ((and (symbolp (car cl))
		  (consp (cdr cl))
		  (null (cddr cl)))
	     `(let ((,(car cl) ,(cadr cl)))
		(and ,(car cl)
		     (and-let* ,(cdr clause)
			       ,@body))))
	    (t
	     (error "malformed and-let* binding spec: %s" cl))))))

(defadvice skk-set-henkan-point (around
				 skk-pskana-dakuten-workaround
				 activate compile)
  "\
���꤬�ʤγ���ʸ����������Ⱦ�����ΤȤ�����ǽ�ʤ��
ľ����ʸ���ˤ�����դ�����Τ����꤬�ʤγ���ʸ���ˤ��롣"
  (or (and-let* ((skk-henkan-mode)
		 (c (skk-downcase last-command-event))
		 (next (skk-select-branch
			(or skk-current-rule-tree skk-rule-tree) c))
		 ((null (skk-get-branch-list next)))
		 (s (skk-get-kana next))
		 ((memq s '(skk-kanagaki-dakuten
			    skk-kanagaki-handakuten))))
	(funcall s)
	(skk-set-char-before-as-okurigana)
	t)
      ad-do-it))

;; simplified version of the advice with the same name in nicola/skk-kanagaki.el
(defadvice skk-compute-henkan-lists-sub-adjust-okuri (around
						      skk-kanagaki-adjust-okuri
						      activate compile)
    (let ((item (ad-get-arg 0))
	  (okuri-key (ad-get-arg 1)))
      (setq ad-return-value
	    (if (string-match (concat "^" (regexp-quote okuri-key)) item)
		;; okuri-key �� "��" �� item �� "�ä�" �ʤɤ��ä���硣
		okuri-key
	      item))))


;;; Initalization

(defun skk-pskana-init ()
  (interactive)
  (setq skk-downcase-alist
	(symbol-value (intern (format "skk-pskana-downcase-alist-%s"
				      skk-pskana-keyboard-type))))

  (setq skk-set-henkan-point-key
	(symbol-value (intern (format "skk-pskana-set-henkan-point-key-%s"
				      skk-pskana-keyboard-type))))

  (setq skk-previous-candidate-char
	(symbol-value (intern (format "skk-pskana-previous-candidate-char-%s"
				      skk-pskana-keyboard-type))))

  (setq skk-rom-kana-base-rule-list 
	(append
	 (symbol-value (intern (format "skk-pskana-rom-kana-base-rule-list-%s-%s"
				       skk-pskana-keyboard-layout
				       skk-pskana-keyboard-type)))
	 (symbol-value (intern (format "skk-pskana-rom-kana-rule-list-%s-%s"
				       skk-pskana-keyboard-layout
				       skk-pskana-keyboard-type)))))

  (setq skk-rom-kana-rule-list nil))

(skk-pskana-init)

(provide 'skk-pskana)
