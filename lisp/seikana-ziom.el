;;; -*- coding: utf-8; lexical-binding: t -*-
;;; seikana-ziom.el - 字音かなづかひユーティリティ
;;;
;;;  Copyright (c) 2006 OOHASHI Daichi, All rights reserved.
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;
;;;  3. Neither the name of the authors nor the names of its contributors
;;;     may be used to endorse or promote products derived from this
;;;     software without specific prior written permission.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(require 'cl-lib)

(defconst seikana-ryakuzi-seizi-alist
  '(("亜" "亞")
    ("悪" "惡")
    ("圧" "壓")
    ("囲" "圍")
    ("為" "爲")
    ("医"
     ("医" . "うつぼ。矢を亡（か）くす。")
     ("醫" . "醫者"))
    ("壱" "壹")
    ("稲" "稻")
    ("飲" "飮")
    ("隠" "隱")
    ("営" "營")
    ("栄" "榮")
    ("衛" "衞")
    ("駅" "驛")
    ("円" "圓")
    ("艶" "艷")
    ("塩" "鹽")
    ("奥" "奧")
    ("応" "應")
    ("欧" "歐")
    ("殴" "毆")
    ("穏" "穩")
    ("仮" "假")
    ("価" "價")
    ("画" "畫")
    ("会" "會")
    ("壊" "壞")
    ("懐" "懷")
    ("絵" "繪")
    ("拡" "擴")
    ("殻" "殼")
    ("覚" "覺")
    ("学" "學")
    ("岳"
     ("岳" . "[形]尊敬すべきもの。岳父")
     ("嶽" . "高い山"))
    ("楽" "樂")
    ("勧" "勸")
    ("巻" "卷")
    ("歓" "歡")
    ("缶"
     ("缶" . "ほとぎ")
     ("罐" . "水を汲む器"))
    ("観" "觀")
    ("関" "關")
    ("陥" "陷")
    ("巌" "巖")
    ("顔" "顏")
    ("帰" "歸")
    ("気" "氣")
    ("亀" "龜")
    ("偽" "僞")
    ("戯" "戲")
    ("犠" "犧")
    ("旧" "舊")
    ("拠" "據")
    ("挙" "擧")
    ("峡" "峽")
    ("挟" "挾")
    ("狭" "狹")
    ("暁" "曉")
    ("区" "區")
    ("駆" "驅")
    ("勲" "勳")
    ("径" "徑")
    ("恵" "惠")
    ("渓" "溪")
    ("経" "經")
    ("継" "繼")
    ("茎" "莖")
    ("蛍" "螢")
    ("軽" "輕")
    ("鶏" "鷄")
    ("芸"
     ("芸" . "ヘンルーダ ※三畫の草冠の字は別字とも")
     ("藝" . "わざ"))
    ("欠"
     ("欠" . "あくび。からだがまがる")
     ("缺" . "かける。ゑぐる"))
    ("倹" "儉")
    ("剣" "劍")
    ("圏" "圈")
    ("検" "檢")
    ("権" "權")
    ("献" "獻")
    ("県"
     ("県" . "首をさかさにつりさげる")
     ("縣" . "あがた"))
    ("険" "險")
    ("顕" "顯")
    ("験" "驗")
    ("厳" "嚴")
    ("効" "效") ; もとは別字らしい
    ("広" "廣")
    ("恒" "恆")
    ("鉱" "鑛")
    ("号" "號")
    ("国" "國")
    ("済" "濟")
    ("砕" "碎")
    ("斎" "齋")
    ("剤" "劑")
    ("桜" "櫻")
    ("冊" "册")
    ("雑" "雜")
    ("参" "參")
    ("惨" "慘")
    ("桟" "棧")
    ("蚕"
     ("蚕" . "みみず")
     ("蠶" . "かいこ"))
    ("賛" "贊")
    ("残" "殘")
    ("糸"
     ("糸" . "いとすぢ")
     ("絲" . "糸をよりあはせたもの"))
    ("歯" "齒")
    ("児" "兒")
    ("辞" "辭")
    ("湿" "濕")
    ("実" "實")
    ("舎" "舍")
    ("写" "寫")
    ("釈" "釋")
    ("寿" "壽")
    ("収" "收")
    ("従" "從")
    ("渋" "澁")
    ("獣" "獸")
    ("縦" "縱")
    ("粛" "肅")
    ("処" "處")
    ("叙" "敍")
    ("奨" "奬")
    ("将" "將")
    ("焼" "燒")
    ("称" "稱")
    ("証"
     ("証" . "いさめてあやまりをただす")
     ("證" . "あかす"))
    ("乗" "乘")
    ("剰" "剩")
    ("壌" "壤")
    ("嬢" "孃")
    ("条" "條")
    ("浄" "淨")
    ("畳" "疊")
    ("穣" "穰")
    ("譲" "讓")
    ("醸" "釀")
    ("嘱" "囑")
    ("触" "觸")
    ("寝" "寢")
    ("慎" "愼")
    ("晋" "晉")
    ("真" "眞")
    ("尽" "盡")
    ("図" "圖")
    ("粋" "粹")
    ("酔" "醉")
    ("随" "隨")
    ("髄" "髓")
    ("数" "數")
    ("枢" "樞")
    ("声" "聲")
    ("静" "靜")
    ("斉" "齊")
    ("摂" "攝")
    ("窃" "竊")
    ("専" "專")
    ("戦" "戰")
    ("浅" "淺")
    ("潜" "潛")
    ("繊" "纖")
    ("践" "踐")
    ("銭" "錢")
    ("禅" "禪")
    ("双" "雙")
    ("壮" "壯")
    ("捜" "搜")
    ("挿" "插")
    ("争" "爭")
    ("総" "總")
    ("聡" "聰")
    ("荘" "莊")
    ("装" "裝")
    ("騒" "騷")
    ("臓" "臟")
    ("蔵" "藏")
    ("属" "屬")
    ("続" "續")
    ("堕" "墮")
    ("体"
     ("体" . "おとる。あらい。體の略字")
     ("體" . "からだ"))
    ("対" "對")
    ("帯" "帶")
    ("滞" "滯")
    ("台"
     ("台" . "タイ: 星の名。敬稱。イ: 一人稱")
     ("臺" . "うてな"))
    ("滝" "瀧")
    ("択" "擇")
    ("沢" "澤")
    ("単" "單")
    ("担" "擔")
    ("胆" "膽")
    ("団" "團")
    ("弾" "彈")
    ("断" "斷")
    ("痴" "癡")
    ("遅" "遲")
    ("昼" "晝")
    ("虫"
     ("虫" . "まむし。爬虫類")
     ("蟲" . "むし"))
    ("鋳" "鑄")
    ("庁" "廳")
    ("聴" "聽")
    ("鎮" "鎭")
    ("逓" "遞")
    ("鉄"
     ("鉄" . "ぬう。鐵の略字")
     ("鐵" . "くろがね"))
    ("転" "轉")
    ("点" "點")
    ("伝" "傳")
    ("党"
     ("党" . "姓・民族名")
     ("黨" . "なかま。やから"))
    ("盗" "盜")
    ("灯"
     ("灯" . "はげしい火")
     ("燈" . "ともしび"))
    ("当" "當")
    ("闘" "鬪")
    ("独" "獨")
    ("読" "讀")
    ("届" "屆")
    ("縄" "繩")
    ("弐" "貳")
    ("悩" "惱")
    ("脳" "腦")
    ("廃" "廢")
    ("拝" "拜")
    ("売" "賣")
    ("麦" "麥")
    ("発" "發")
    ("髪" "髮")
    ("抜" "拔")
    ("蛮" "蠻")
    ("秘" "祕")
    ("浜"
     ("浜" . "小さな水路")
     ("濱" . "はま"))
    ("払" "拂")
    ("仏" "佛")
    ("並" "竝")
    ("変" "變")
    ("辺" "邊")
    ("弁"
     ("弁" . "かんむり")
     ("辨" . "よりわける")
     ("辧" . "辨の異體字")
     ("辯" . "うまくものを言ふ")
     ("瓣" . "はなびら"))
    ("舗" "舖")
    ("穂" "穗")
    ("宝" "寶")
    ("豊" "豐")
    ("没" "沒")
    ("万" "萬") ; 万は卍の變形
    ("満" "滿")
    ("黙" "默")
    ("弥" "彌")
    ("薬" "藥")
    ("訳" "譯")
    ("薮" "藪")
    ("予"
     ("予" . "あたへる。われ=余")
     ("豫" . "あらかじめ"))
    ("余"
     ("余" . "われ=余")
     ("餘" . "あまる"))
    ("与" "與")
    ("誉" "譽")
    ("揺" "搖")
    ("様" "樣")
    ("謡" "謠")
    ("来" "來")
    ("乱" "亂")
    ("覧" "覽")
    ("竜" "龍")
    ("両" "兩")
    ("猟" "獵")
    ("塁" "壘")
    ("励" "勵")
    ("礼" "禮") ; つくりは豐にあらず
    ("隷" "隸")
    ("霊" "靈")
    ("齢" "齡")
    ("恋" "戀")
    ("炉" "爐")
    ("労" "勞")
    ("楼" "樓")
    ("禄" "祿")
    ("湾" "灣"))
  "新字體の漢字と康煕字典體の漢字群の聯想リスト
康煕字典體の漢字が複數ある場合
\(\"漢字\" . \"説明文\"\) のかたちで説明文をふくむ")

(defconst seikana-ryakuzi-seizi-table
  (let ((table (make-hash-table :test 'equal)))
    (mapc (lambda (xs)
            (puthash (car xs) (cdr xs) table))
          seikana-ryakuzi-seizi-alist)
    table)
  "新字體の漢字から康煕字典體の漢字への對應表。値には説明文を含む")

(defconst seikana-seizi-ryakuzi-table
  (let ((table (make-hash-table :test 'equal)))
    (mapc (lambda (xs)
            (let ((r (car xs)))
              (mapc (lambda (s)
                      (puthash (if (consp s) (car s) s)
                               (list r)
                               table))
                    (cdr xs))))
          seikana-ryakuzi-seizi-alist)
    table)
  "康煕字典體の漢字から新字體の漢字への對應表")

(defconst seikana-ryakuzi-pat
  (regexp-opt (mapcar #'car seikana-ryakuzi-seizi-alist))
  "康煕字典體から變化のあった新字體の漢字にマッチするパターン")

(defconst seikana-seizi-pat
  (regexp-opt
   (mapcar (lambda (x)
             (if (consp x)
                 (car x)
               x))
           (apply #'append
                  (mapcar #'cdr seikana-ryakuzi-seizi-alist))))
  "新字體にあらためられた康煕字典體の漢字にマッチするパターン")

;; These data are based on that of Heisei Gimon Kanadukai.
;;   http://homepage3.nifty.com/gimon/
(defconst seikana-kanzi-ziom-table
  (let ((table (make-hash-table)))
    (mapc
     (lambda (xs)
       (mapc (lambda (zi)
               (let ((v (gethash zi table ())))
                 (puthash zi (cons (car xs) v) table)))
             (cdr xs)))
     '(("ア"    ?閼 ?阿 ?婀 ?痾 ?蛙 ?亞 ?亜 ?唖 ?錏 ?堊 ?鴉)
       ("アイ"  ?藹 ?靄 ?娃 ?哇 ?鞋 ?隘 ?哀 ?愛 ?曖 ?靉 ?挨 ?埃 ?欸)
       ("アク"  ?幄 ?齷 ?握 ?渥 ?惡 ?悪 ?軛 ?阨)
       ("アツ"  ?遏 ?閼 ?斡 ?軋 ?壓 ?圧)
       ("アム"  ?暗 ?闇 ?諳 ?黯 ?菴 ?庵 ?罨 ?餡)
       ("アン"  ?安 ?按 ?案 ?鞍 ?鮟 ?晏 ?杏 ?行)
       ("イ"    ?易 ?椅 ?猗 ?倚 ?移 ?恚 ?縊 ?伊 ?夷 ?姨 ?痍 ?惟 ?唯 ?遺 ?彝 ?彜 ?懿 ?肄 ?以 ?苡 ?意 ?異 ?已 ?怡 ?貽 ?矣 ?醫 ?衣 ?依)
       ("(イ)"  ?医)
       ("アイ"  ?医)
       ("エイ"  ?医)
       ("ヰ"    ?委 ?萎 ?逶 ?痿 ?爲 ?為 ?位 ?維 ?帷 ?威 ?尉 ?慰 ?畏 ?胃 ?謂 ?渭 ?蝟 ?彙 ?韋 ?偉 ?緯 ?葦 ?違 ?圍 ?囗 ?囲 ?幃)
       ("ヰキ"  ?域)
       ("イク"  ?育 ?毓)
       ("ヰク"  ?郁)
       ("イチ"  ?一 ?弌 ?壹 ?壱)
       ("イツ"  ?一 ?弌 ?溢 ?鎰 ?逸 ?乙 ?佚 ?軼 ?聿 ?鴪 ?鷸)
       ("イム"  ?陰 ?蔭 ?音 ?恁 ?婬 ?淫 ?霪 ?飮 ?飲)
       ("イン"  ?印 ?因 ?氤 ?姻 ?咽 ?引 ?蚓 ?胤 ?酳 ?寅 ?堙 ?湮 ?允 ?尹 ?殷 ?慇 ?隱 ?隠)
       ("ヰン"  ?贇 ?員 ?韻 ?韵 ?均 ?殞 ?隕 ?院)
       ("ウ"    ?羽 ?雨 ?于 ?芋 ?宇 ?迂 ?盂 ?紆 ?傴 ?禹 ?齲 ?烏 ?胡 ?右 ?佑 ?祐 ?有 ?侑)
       ("ウイ"  ?茴)
       ("ウツ"  ?鬱 ?欝 ?熨 ?蔚)
       ("ウン"  ?云 ?芸 ?紜 ?耘 ?雲 ?繧 ?運 ?暈 ?慍 ?蘊 ?薀 ?饂)
       ("ウム"  ?吽)
       ("エ"    ?衣 ?依 ?哇)
       ("ヱ"    ?慧 ?惠 ?恵 ?會 ?会 ?繪 ?絵 ?畫 ?画 ?囘 ?回 ?穢 ?哇)
       ("エィ"  ?影 ?霙 ?映 ?暎 ?英 ?瑛 ?嬰 ?纓 ?瓔 ?珱 ?盈 ?楹 ?營 ?営 ?塋 ?瑩 ?潁 ?頴 ?穎 ?贏 ?瀛 ?郢)
       ("エイ"  ?殪 ?翳 ?曳 ?曵 ?洩 ?鋭 ?睿 ?叡 ?裔)
       ("ヱィ"  ?永 ?泳 ?詠 ?咏 ?榮 ?栄 ?蠑)
       ("ヱイ"  ?衞 ?衛)
       ("エキ"  ?易 ?蜴 ?液 ?掖 ?腋 ?疫 ?役 ?益 ?亦 ?奕 ?懌 ?繹 ?驛 ?駅)
       ("エツ"  ?謁 ?咽 ?悦 ?閲)
       ("ヱツ"  ?戉 ?越 ?鉞 ?曰 ?粤)
       ("エム"  ?厭 ?魘 ?黶 ?奄 ?掩 ?俺 ?閹 ?淹 ?炎 ?簷 ?艷 ?艶 ?閻 ?焔 ?鹽 ?塩)
       ("エン"  ?偃 ?堰 ?宴 ?咽 ?煙 ?烟 ?燕 ?嚥 ?臙 ?讌 ?延 ?筵 ?莚 ?蜒 ?涎 ?沿 ?鉛 ?演 ?鳶 ?悁 ?捐 ?掾 ?縁 ?衍 ?焉 ?嫣)
       ("ヱン"  ?鴛 ?怨 ?苑 ?宛 ?婉 ?鋺 ?蜿 ?豌 ?垣 ?寃 ?冤 ?爰 ?援 ?媛 ?湲 ?袁 ?園 ?薗 ?遠 ?轅 ?猿 ?淵 ?渕 ?渊 ?圓 ?円 ?圜 ?娟)
       ("ヲ"    ?烏 ?嗚 ?塢 ?於 ?汚 ?乎 ?惡 ?悪 ?和)
       ("オ"    ?於 ?淤)
       ("ヲゥ"  ?翁 ?蓊 ?鶲)
       ("アゥ"  ?央 ?怏 ?殃 ?秧 ?鞅 ?泱 ?鴦 ?罌 ?嚶 ?櫻 ?桜 ?鸚 ?鶯 ?鴬)
       ("アウ"  ?奧 ?奥 ?襖 ?墺 ?懊 ?澳 ?鏖)
       ("アフ"  ?凹 ?押 ?鴨)
       ("オゥ"  ?鷹 ?應 ?応)
       ("オウ"  ?鴎 ?嘔 ?歐 ?欧 ?毆 ?殴 ?甌 ?謳)
       ("オフ"  ?邑)
       ("ワゥ"  ?往 ?徃 ?王 ?旺 ?枉 ?汪 ?黄 ?横 ?皇 ?凰 ?泓)
       ("ヲク"  ?屋 ?奧 ?奥)
       ("オク"  ?億 ?憶 ?臆 ?檍)
       ("ヲツ"  ?榲 ?膃)
       ("オツ"  ?乙)
       ("ヲン"  ?怨 ?苑 ?遠 ?温 ?瘟 ?鰛 ?鰮 ?褞 ?穩 ?穏)
       ("オム"  ?音)
       ("オン"  ?隱 ?隠 ?恩)
       ("カ"    ?佳 ?可 ?河 ?珂 ?呵 ?柯 ?舸 ?訶 ?軻 ?何 ?荷 ?苛 ?哥 ?歌 ?個 ?个 ?箇 ?下 ?加 ?嘉 ?架 ?茄 ?迦 ?枷 ?珈 ?痂 ?笳 ?跏 ?伽 ?夏 ?廈 ?厦 ?家 ?稼 ?嫁 ?蝦 ?瑕 ?鰕 ?暇 ?假 ?仮 ?霞 ?葭 ?遐 ?罅 ?谺 ?賈 ?價 ?価)
       ("クヮ"  ?卦 ?果 ?菓 ?課 ?夥 ?顆 ?裹 ?踝 ?火 ?禾 ?科 ?蝌 ?過 ?渦 ?禍 ?堝 ?窩 ?蝸 ?戈 ?找 ?譌 ?瓜 ?寡 ?華 ?崋 ?樺 ?譁 ?嘩 ?匕 ?化 ?花 ?貨 ?靴 ?訛 ?囮)
       ("ガ"    ?我 ?俄 ?峨 ?峩 ?蛾 ?餓 ?哦 ?娥 ?莪 ?鵝 ?鵞 ?伽 ?賀 ?駕 ?牙 ?芽 ?雅 ?呀 ?訝 ?衙)
       ("グヮ"  ?畫 ?画 ?臥 ?瓦)
       ("カイ"  ?丐 ?解 ?觧 ?蟹 ?蠏 ?廨 ?懈 ?邂 ?拐 ?街 ?介 ?芥 ?价 ?疥 ?界 ?堺 ?畍 ?堺 ?戒 ?械 ?誡 ?皆 ?階 ?偕 ?揩 ?楷 ?諧 ?薤 ?亥 ?骸 ?改 ?海 ?開 ?凱 ?醢)
       ("クヮイ"        ?會 ?会 ?獪 ?繪 ?絵 ?膾 ?薈 ?檜 ?桧 ?挂 ?掛 ?褂 ?怪 ?恠 ?乖 ?壞 ?壊 ?懷 ?懐 ?快 ?塊 ?魁 ?傀 ?槐 ?瑰 ?嵬 ?隗 ?悔 ?晦 ?誨 ?灰 ?恢 ?詼 ?潰 ?囘 ?回 ?廻 ?徊 ?茴 ?蛔 ?迴 ?喙)
       ("ガイ"  ?害 ?蓋 ?盖 ?葢 ?街 ?崖 ?崕 ?啀 ?睚 ?涯 ?亥 ?劾 ?咳 ?該 ?垓 ?孩 ?骸 ?駭 ?慨 ?概 ?漑 ?鎧 ?凱 ?剴 ?皚 ?愾 ?礙 ?碍 ?乂 ?艾)
       ("グヮイ"        ?外 ?磑)
       ("カク"  ?確 ?鶴 ?角 ?埆 ?桷 ?殼 ?殻 ?愨 ?較 ?覺 ?覚 ?攪 ?撹 ?各 ?恪 ?閣 ?擱 ?格 ?挌 ?骼 ?客 ?喀 ?咯 ?壑 ?赫 ?嚇 ?核 ?革 ?覈 ?鬲 ?隔 ?膈)
       ("クヮク"        ?矍 ?攫 ?钁 ?蠖 ?穫 ?獲 ?郭 ?椁 ?廓 ?槨 ?擴 ?拡 ?霍 ?癨 ?畫 ?画 ?劃 ?馘 ?掴 ?幗)
       ("ガク"  ?岳 ?學 ?斈 ?学 ?嶽 ?樂 ?楽 ?咢 ?鰐 ?顎 ?愕 ?萼 ?蕚 ?諤 ?鄂 ?齶 ?壑 ?額)
       ("カツ"  ?曷 ?喝 ?褐 ?蝎 ?葛 ?鞨 ?渇 ?蠍 ?羯 ?轄 ?瞎 ?割 ?戛 ?戞 ?黠)
       ("クヮツ"        ?括 ?筈 ?聒 ?蛞 ?活 ?闊 ?濶 ?刮 ?豁 ?滑 ?猾)
       ("グヮツ"        ?月)
       ("カム"  ?勘 ?堪 ?戡 ?函 ?凾 ?涵 ?坎 ?敢 ?橄 ?瞰 ?甘 ?柑 ?坩 ?疳 ?蚶 ?拑 ?箝 ?鉗 ?嵌 ?歛 ?咸 ?緘 ?鹹 ?感 ?憾 ?撼 ?轗 ?喊 ?歉 ?陷 ?陥 ?監 ?艦 ?鑑 ?鑒 ?檻 ?銜)
       ("カン"  ?乾 ?幹 ?翰 ?瀚 ?韓 ?侃 ?寒 ?干 ?刊 ?刋 ?汗 ?竿 ?肝 ?奸 ?扞 ?杆 ?桿 ?罕 ?鼾 ?旱 ?悍 ?捍 ?稈 ?駻 ?  ?骭 ?漢 ?艱 ?看 ?栞 ?邯 ?姦 ?菅 ?間 ?簡 ?嫺 ?癇 ?澗 ?閑 ?嫻 ?柬 ?揀 ?諫 ?諌 ?羹 ?羮 ?甲)
       ("クヮン"        ?完 ?冠 ?浣 ?澣 ?莞 ?皖 ?官 ?棺 ?管 ?館 ?舘 ?菅 ?寛 ?桓 ?款 ?緩 ?湲 ?貫 ?慣 ?奐 ?換 ?喚 ?渙 ?煥 ?歡 ?歓 ?罐 ?懽 ?觀 ?観 ?讙 ?鑵 ?驩 ?灌 ?潅 ?勸 ?勧 ?顴 ?環 ?寰 ?鐶 ?鬟 ?圜 ?  ?還 ?串 ?患 ?丱 ?關 ?関 ?宦 ?鰥 ?卷 ?巻)
       ("(クヮン)" ?缶)
       ("フ" ?缶)
       ("フウ" ?缶)
       ("ガム"  ?含 ?頷 ?龕 ?嵒 ?癌 ?岩 ?巖 ?巌)
       ("ガン"  ?岸 ?雁 ?贋 ?顏 ?顔 ?鳫 ?鴈 ?眼)
       ("グヮン"        ?願 ?元 ?玩 ?翫 ?頑 ?丸)
       ("キ"    ?企 ?伎 ?岐 ?跂 ?奇 ?竒 ?寄 ?騎 ?崎 ?嵜 ?埼 ?碕 ?剞 ?掎 ?畸 ?綺 ?規 ?窺 ?槻 ?曦 ?枳 ?羈 ?羇 ?覊 ?器 ?噐 ?棄 ?弃 ?季 ?悸 ?祁 ?冀 ?驥 ?几 ?机 ?飢 ?肌 ?屓 ?癸 ?揆 ?葵 ?耆 ?覬 ?龜 ?亀 ?喜 ?嬉 ?僖 ?憙 ?熹 ?禧 ?己 ?忌 ?紀 ?記 ?起 ?杞 ?其 ?基 ?期 ?棋 ?棊 ?箕 ?朞 ?淇 ?祺 ?騏 ?麒 ?旗 ?煕 ?熈 ?姫 ?希 ?稀 ?唏 ?晞 ?欷 ?幾 ?畿 ?磯 ?機 ?譏 ?饑 ?既 ?曁 ?毅 ?汽 ?氣 ?気 ?祈 ?圻 ?旡)
       ("クヰ"  ?危 ?詭 ?跪 ?毀 ?燬 ?麾 ?虧 ?喟 ?逵 ?馗 ?軌 ?揮 ?輝 ?暉 ?徽 ?貴 ?簣 ?饋 ?匱 ?櫃 ?鬼 ?愧 ?餽 ?卉 ?歸 ?帰 ?皈)
       ("ギ"    ?伎 ?妓 ?技 ?宜 ?誼 ?祇 ?義 ?儀 ?蟻 ?議 ?礒 ?艤 ?羲 ?犧 ?犠 ?戲 ?戯 ?葵 ?欺 ?疑 ?擬 ?沂)
       ("グヰ"  ?僞 ?偽 ?魏 ?巍)
       ("キク"  ?菊 ?掬 ?鞠 ?麹 ?鞫)
       ("キチ"  ?吉 ?橘)
       ("キツ"  ?吉 ?詰 ?佶 ?拮 ?橘 ?乞 ?吃 ?訖 ?屹 ?喫)
       ("キャ"  ?伽 ?脚)
       ("キャク"        ?却 ?卻 ?脚 ?客)
       ("ギャク"        ?虐 ?瘧 ?謔 ?逆)
       ("キュゥ"        ?宮 ?弓 ?穹 ?躬 ?窮)
       ("キウ"  ?鬮 ?臼 ?舅 ?舊 ?旧 ?丘 ?蚯 ?邱 ?久 ?灸 ?玖 ?柩 ?疚 ?休 ?貅 ?烋 ?朽 ?求 ?救 ?球 ?毬 ?裘 ?逑 ?九 ?仇 ?究 ?鳩 ?咎 ?嗅 ?廏 ?廐 ?厩 ?糾 ?糺 ?赳 ?樛)
       ("キフ"  ?及 ?吸 ?汲 ?笈 ?級 ?急 ?泣 ?給 ?翕 ?歙)
       ("ギウ"  ?牛)
       ("キョ"  ?去 ?居 ?鋸 ?据 ?裾 ?倨 ?踞 ?巨 ?拒 ?渠 ?距 ?炬 ?秬 ?苣 ?鉅 ?虚 ?嘘 ?墟 ?歔 ?車 ?據 ?拠 ?醵 ?遽 ?擧 ?挙 ?舉 ?欅 ?筥 ?許)
       ("ギョ"  ?魚 ?漁 ?御 ?禦 ?圄 ?圉 ?馭)
       ("キャゥ"        ?享 ?亨 ?強 ?繦 ?襁 ?郷 ?響 ?饗 ?嚮 ?卿 ?香 ?僵 ?薑 ?彊 ?強 ?疆 ?姜 ?羌 ?杏 ?京 ?亰 ?驚 ?梗 ?竟 ?鏡 ?競 ?竸 ?境 ?馨 ?經 ?経)
       ("キョゥ"        ?共 ?供 ?恭 ?拱 ?蛬 ?凶 ?兇 ?匈 ?胸 ?恟 ?洶 ?恐 ?蛩 ?鞏 ?跫 ?廾 ?興 ?兢 ?矜)
       ("クヰャゥ"      ?匡 ?框 ?筐 ?筺 ?狂 ?誑 ?兄 ?況 ?况)
       ("ケウ"  ?叫 ?梟 ?竅 ?徼 ?喬 ?轎 ?橋 ?僑 ?矯 ?蕎 ?嬌 ?驕 ?教 ?餃)
       ("ケフ"  ?夾 ?峽 ?峡 ?狹 ?狭 ?筴 ?陜 ?挾 ?挟 ?侠 ?頬 ?篋 ?莢 ?怯 ?脇 ?脅 ?協 ?叶)
       ("ギャゥ"        ?仰 ?行 ?形)
       ("ギョゥ"        ?凝)
       ("ゲウ"  ?尭 ?嶢 ?僥 ?曉 ?暁 ?澆 ?驍 ?翹 ?蟯)
       ("ゲフ"  ?業)
       ("キョク"        ?旭 ?局 ?跼 ?曲 ?髷 ?亟 ?極 ?棘 ?蕀)
       ("クヰョク"      ?洫)
       ("ギョク"        ?玉 ?嶷)
       ("キム"  ?禁 ?襟 ?噤 ?金 ?錦 ?欽 ?今 ?琴 ?衿 ?衾 ?禽 ?檎 ?擒)
       ("キン"  ?巾 ?緊 ?釁 ?均 ?鈞 ?斤 ?芹 ?近 ?忻 ?釿 ?欣 ?掀 ?筋 ?菫 ?懃 ?勤 ?謹 ?槿 ?瑾 ?僅 ?覲 ?饉 ?矜)
       ("クヰン"        ?窘 ?菌 ?麕 ?箘 ?箟)
       ("ギム"  ?吟 ?崟)
       ("ギン"  ?銀 ?垠 ?齦 ?憖)
       ("ク"    ?宮 ?工 ?功 ?紅 ?貢 ?箜 ?供 ?廾 ?倶 ?惧 ?句 ?駒 ?劬 ?煦 ?枸 ?狗 ?佝 ?矩 ?區 ?区 ?躯 ?驅 ?駆 ?駈 ?嶇 ?吁 ?瞿 ?懼 ?衢 ?窶 ?苦 ?庫 ?久 ?玖 ?九 ?鳩 ?口 ?垢 ?吼)
       ("グ"    ?具 ?惧 ?颶 ?愚 ?虞 ?麌 ?弘)
       ("クゥ"  ?空)
       ("グゥ"  ?宮)
       ("グウ"  ?禺 ?嵎 ?寓 ?遇 ?隅 ?偶 ?藕)
       ("クツ"  ?屈 ?掘 ?崛 ?倔 ?堀 ?窟)
       ("クン"  ?君 ?裙 ?訓 ?熏 ?薫 ?勳 ?勲 ?醺 ?燻 ?葷)
       ("グン"  ?軍 ?羣 ?群 ?郡)
       ("クヱ"  ?圭 ?挂 ?卦 ?掛 ?褂 ?華 ?匕 ?化)
       ("ケ"    ?稀 ?旡 ?氣 ?気 ?懈 ?芥 ?懸 ?家 ?袈 ?假 ?仮)
       ("ゲ"    ?偈 ?外 ?解 ?觧 ?礙 ?碍 ?下 ?夏 ?牙)
       ("クヱィ"        ?兄 ?冂 ?冏 ?絅 ?迥 ?炯 ?烱)
       ("クヱイ"        ?圭 ?珪 ?桂 ?畦 ?鮭 ?奎 ?袿 ?閨 ?罫 ?挂 ?慧 ?惠 ?恵 ?攜 ?携)
       ("ケィ"  ?京 ?亰 ?勍 ?景 ?憬 ?競 ?竸 ?境 ?卿 ?慶 ?敬 ?警 ?檠 ?頃 ?傾 ?勁 ?痙 ?輕 ?軽 ?頸 ?頚 ?徑 ?径 ?經 ?経 ?逕 ?剄 ?脛 ?莖 ?茎 ?夐 ?瓊 ?馨 ?磬 ?謦 ?形 ?刑 ?型 ?荊 ?螢 ?蛍 ?煢)
       ("ケイ"  ?啓 ?綮 ?契 ?禊 ?系 ?係 ?繋 ?計 ?詣 ?稽 ?兮 ?盻 ?奚 ?溪 ?渓 ?谿 ?蹊 ?鷄 ?鶏 ?笄 ?繼 ?継 ?薊 ?醯 ?憩 ?憇 ?掲)
       ("ゲィ"  ?迎 ?鯨 ?黥)
       ("ゲイ"  ?倪 ?猊 ?貎 ?睨 ?霓 ?鯢 ?麑 ?藝 ?芸 ?囈)
       ("(ゲイ)"        ?芸)
       ("クヱキ"        ?闃)
       ("ゲキ"  ?劇 ?戟 ?隙 ?屐 ?郤 ?撃 ?激 ?檄 ?覡 ?鬩 ?鷁)
       ("グヱキ"        ?闃 ?鵙 ?鴃)
       ("クヱツ"        ?闕 ?厥 ?獗 ?蹶 ?決 ?决 ?訣 ?抉 ?穴 ?血 ?譎)
       ("ケツ"  ?訐 ?歇 ?碣 ?竭 ?頁 ?挈 ?潔 ?纈 ?襭 ?結 ?桀 ?傑 ?缺 ?欠)
       ("(ケツ)"        ?欠)
       ("ケム"  ?欠)
       ("コム"  ?欠)
       ("グヱツ"        ?月)
       ("ゲツ"  ?孑)
       ("クヱン"        ?喧 ?暄 ?諠 ?絢 ?犬 ?縣 ?県 ?懸 ?羂 ?涓 ?鵑 ?鉉 ?拳 ?眷 ?倦 ?捲 ?圈 ?圏 ?券 ?劵 ?綣 ?權 ?権)
       ("(クヱン)"      ?県)
       ("ケウ"  ?県)
       ("ケム"  ?嶮 ?驗 ?験 ?鹸 ?險 ?険 ?檢 ?検 ?儉 ?倹 ?瞼 ?劍 ?剣 ?剱 ?劒 ?劔 ?黔 ?兼 ?謙 ?嫌 ?慊 ?蒹 ?歉)
       ("ケン"  ?建 ?健 ?腱 ?鍵 ?憲 ?軒 ?獻 ?献 ?間 ?繝 ?牽 ?痃 ?研 ?妍 ?肩 ?見 ?硯 ?賢 ?堅 ?慳 ?繭 ?顯 ?顕 ?乾 ?件 ?遣 ?譴 ?惓 ?愆 ?搴 ?謇 ?騫 ?蹇 ?狷 ?絹 ?甄 ?虔)
       ("グヱン"        ?元 ?芫 ?阮 ?原 ?源 ?愿 ?幻 ?玄 ?眩 ?衒 ?拳)
       ("ゲム"  ?驗 ?験 ?嫌 ?減 ?嚴 ?厳 ?儼)
       ("ゲン"  ?言 ?限 ?眼 ?弦 ?絃 ?舷 ?現 ?彦 ?諺)
       ("コ"    ?己 ?杞 ?去 ?虚 ?據 ?拠 ?炬 ?乎 ?呼 ?古 ?姑 ?枯 ?鈷 ?估 ?怙 ?沽 ?罟 ?蛄 ?詁 ?辜 ?鴣 ?故 ?胡 ?糊 ?湖 ?瑚 ?葫 ?蝴 ?餬 ?固 ?錮 ?凅 ?痼 ?個 ?个 ?箇 ?涸 ?庫 ?弧 ?狐 ?呱 ?觚 ?孤 ?菰 ?戸 ?雇 ?顧 ?扈 ?滬 ?股 ?虎 ?乕 ?琥 ?鼓 ?皷 ?瞽 ?冱 ?冴 ?壺 ?壷 ?滸 ?蠱 ?賈 ?夸 ?誇 ?跨 ?袴 ?瓠 ?胯 ?刳)
       ("ゴ"    ?期 ?碁 ?御 ?胡 ?瑚 ?醐 ?五 ?伍 ?吾 ?悟 ?梧 ?唔 ?寤 ?晤 ?牾 ?齬 ?圄 ?語 ?互 ?冱 ?冴 ?沍 ?午 ?忤 ?呉 ?娯 ?誤 ?蜈 ?護 ?後 ?后 ?篌 ?檎)
       ("カゥ"  ?巷 ?港 ?江 ?項 ?扛 ?杠 ?矼 ?缸 ?肛 ?槓 ?腔 ?講 ?降 ?絳 ?仰 ?昂 ?向 ?香 ?岡 ?綱 ?鋼 ?崗 ?康 ?糠 ?慷 ?鱇 ?庚 ?亢 ?抗 ?杭 ?航 ?伉 ?吭 ?頏 ?坑 ?亨 ?行 ?衡 ?絎 ?杏 ?更 ?梗 ?哽 ?硬 ?羹 ?羮 ?幸 ?倖 ?耕 ?畊 ?耿 ?鏗)
       ("カウ"  ?交 ?佼 ?校 ?絞 ?郊 ?咬 ?狡 ?纐 ?蛟 ?鵁 ?效 ?効 ?皎 ?孝 ?酵 ?哮 ?攪 ?撹 ?爻 ?肴 ?淆 ?磽 ?膠 ?好 ?浩 ?晧 ?皓 ?誥 ?靠 ?窖 ?高 ?膏 ?槁 ?犒 ?稾 ?稿 ?蒿 ?敲 ?嚆 ?耗 ?攷 ?考 ?栲 ?巧 ?杲 ?昊 ?槹 ?皋 ?皐 ?睾 ?羔)
       ("カフ"  ?合 ?盒 ?哈 ?閤 ?鴿 ?蛤 ?洽 ?恰 ?袷 ?盍 ?闔 ?溘 ?甲 ?匣 ?狎 ?胛 ?岬 ?閘)
       ("クヮゥ"        ?黄 ?簧 ?廣 ?広 ?壙 ?曠 ?昿 ?黌 ?鑛 ?鉱 ?礦 ?砿 ?絋 ?光 ?恍 ?洸 ?絖 ?胱 ?晃 ?滉 ?皇 ?凰 ?徨 ?惶 ?湟 ?煌 ?篁 ?遑 ?隍 ?鍠 ?荒 ?慌 ?肓 ?紘 ?宏 ?浤)
       ("コゥ"  ?公 ?蚣 ?孔 ?工 ?功 ?攻 ?紅 ?汞 ?訌 ?鴻 ?貢 ?控 ?倥 ?虹 ?洪 ?哄 ?鬨 ?閧 ?講 ?降 ?絳 ?興 ?弘 ?肯 ?肱 ?亙 ?亘 ?恆 ?恒 ?嫦 ?薨)
       ("コウ"  ?熕 ?後 ?侯 ?候 ?喉 ?猴 ?篌 ?勾 ?句 ?厚 ?口 ?釦 ?扣 ?后 ?垢 ?詬 ?逅 ?遘 ?拘 ?佝 ?鉤 ?鈎 ?叩 ?冓 ?構 ?遘 ?溝 ?購 ?媾 ?搆 ?覯 ?吼 ?吽 ?寇 ?冦)
       ("ガゥ"  ?強 ?郷 ?剛)
       ("ガウ"  ?拷 ?號 ?号 ?囂 ?敖 ?傲 ?嗷 ?熬 ?螯 ?遨 ?鼇 ?毫 ?豪 ?壕 ?濠)
       ("ガフ"  ?合)
       ("グヮゥ"        ?轟 ?軣)
       ("ゴフ"  ?合 ?盒 ?哈 ?業 ?劫 ?刧)
       ("コク"  ?谷 ?哭 ?斛 ?槲 ?轂 ?穀 ?告 ?梏 ?鵠 ?酷 ?石 ?克 ?剋 ?尅 ?刻 ?黒 ?國 ?圀 ?囗 ?国)
       ("ゴク"  ?獄 ?極)
       ("コツ"  ?乞 ?骨 ?榾 ?鶻 ?兀 ?笏 ?忽 ?惚)
       ("コム"  ?金 ?欽 ?今 ?紺)
       ("コン"  ?建 ?獻 ?献 ?困 ?梱 ?悃 ?坤 ?巛 ?昏 ?婚 ?昆 ?混 ?崑 ?棍 ?焜 ?鯤 ?菎 ?魂 ?壼 ?渾 ?褌 ?琿 ?鶤 ?諢 ?溷 ?袞 ?滾 ?鯀 ?艮 ?恨 ?跟 ?很 ?根 ?痕 ?狠 ?墾 ?懇)
       ("ゴム"  ?嚴 ?厳)
       ("ゴン"  ?勤 ?欣 ?言 ?權 ?権)
       ("サ"    ?左 ?ナ ?佐 ?嵯 ?嵳 ?瑳 ?搓 ?磋 ?蹉 ?差 ?嗟 ?槎 ?唆 ?梭 ?鎖 ?瑣 ?些 ?叉 ?釵 ?査 ?渣 ?砂 ?紗 ?沙 ?裟 ?鯊 ?莎 ?娑 ?詐 ?鮓 ?作 ?茶 ?嗄)
       ("ザ"    ?坐 ?挫 ?座 ?蓙)
       ("サイ"  ?妻 ?犀 ?細 ?顋 ?腮 ?西 ?洒 ?切 ?齊 ?斉 ?濟 ?済 ?齋 ?斎 ?儕 ?歳 ?祭 ?際 ?蔡 ?最 ?債 ?灑 ?殺 ?砦 ?柴 ?倅 ?伜 ?淬 ?碎 ?砕 ?崔 ?催 ?摧 ?再 ?塞 ?賽 ?寨 ?宰 ?縡 ?滓 ?才 ?財 ?戝 ?豺 ?犲 ?栽 ?裁 ?載 ?哉 ?災 ?巛 ?采 ?綵 ?採 ?彩 ?菜 ?猜)
       ("ザイ"  ?劑 ?剤 ?罪 ?在 ?材 ?財 ?戝)
       ("サク"  ?朔 ?槊 ?愬 ?扠 ?搾 ?削 ?索 ?作 ?昨 ?柞 ?筰 ?酢 ?炸 ?窄 ?咋 ?錯 ?醋 ?鑿 ?策 ?册 ?冊 ?柵 ?嘖 ?簀)
       ("サツ"  ?薩 ?撒 ?拶 ?撮 ?刷 ?刹 ?殺 ?察 ?擦 ?扎 ?札 ?紮 ?册 ?冊 ?颯 ?箚)
       ("ザツ"  ?雜 ?雑 ?襍)
       ("サム"  ?參 ?参 ?慘 ?惨 ?驂 ?蠶 ?蚕 ?三 ?參 ?参 ?槧 ?杉 ?衫 ?纔 ?巉 ?讒 ?芟)
       ("(サム)"        ?蚕)
       ("テン"  ?蚕)
       ("サン"  ?傘 ?散 ?繖 ?撒 ?霰 ?珊 ?跚 ?刪 ?餐 ?粲 ?燦 ?戔 ?盞 ?棧 ?桟 ?贊 ?賛 ?讚 ?讃 ?攅 ?鑽 ?鑚 ?算 ?纂 ?簒 ?酸 ?蒜 ?爨 ?纉 ?潸 ?閂 ?山 ?汕 ?産)
       ("ザム"  ?慘 ?惨 ?斬 ?嶄 ?暫 ?慙 ?慚 ?塹 ?懺 ?懴)
       ("ザン"  ?殘 ?残 ?竄)
       ("シ"    ?此 ?紫 ?雌 ?呰 ?疵 ?眥 ?眦 ?貲 ?觜 ?嘴 ?匙 ?支 ?翅 ?肢 ?枝 ?斯 ?廝 ?厮 ?撕 ?氏 ?紙 ?帋 ?賜 ?只 ?咫 ?弛 ?施 ?侈 ?卮 ?巵 ?啻 ?揣 ?朿 ?刺 ?豕 ?四 ?泗 ?駟 ?師 ?獅 ?旨 ?指 ?脂 ?嗜 ?死 ?屍 ?私 ?至 ?鵄 ?次 ?茨 ?姿 ?資 ?恣 ?瓷 ?咨 ?諮 ?示 ?視 ?自 ?矢 ?尸 ?屎 ?摯 ?鷙 ?祗 ?砥 ?鴟 ?肆 ?謚 ?諡 ?使 ?司 ?伺 ?嗣 ?詞 ?飼 ?笥 ?祠 ?史 ?駛 ?士 ?仕 ?志 ?誌 ?痣 ?始 ?子 ?孜 ?仔 ?市 ?柿 ?姉 ?思 ?偲 ?止 ?齒 ?歯 ?址 ?沚 ?趾 ?阯 ?祉 ?徙 ?詩 ?畤 ?塒 ?試 ?之 ?芝 ?巳 ?祀 ?孳 ?幟 ?熾 ?廁 ?厠 ?滓 ?梓 ?竢 ?絲 ?糸 ?耜 ?蚩 ?嗤 ?錙 ?輜 ?緇)
       ("シ"    ?糸)
       ("(シ)"  ?糸)
       ("ベキ"  ?糸)
       ("ミャク"        ?糸)
       ("ジ"    ?爾 ?尓 ?璽 ?兒 ?児 ?示 ?自 ?二 ?弍 ?貳 ?貮 ?弐 ?次 ?仕 ?事 ?亊 ?似 ?字 ?寺 ?侍 ?恃 ?時 ?蒔 ?而 ?轜 ?耳 ?餌 ?珥 ?茲 ?慈 ?滋 ?磁 ?辭 ?辞)
       ("ヂ"    ?地 ?尼 ?怩 ?膩 ?持 ?痔 ?峙 ?治 ?除)
       ("シイ"  ?弑)
       ("シキ"  ?式 ?織 ?識 ?色)
       ("ジキ"  ?食)
       ("ヂキ"  ?直)
       ("ジク"  ?衄 ?衂)
       ("ヂク"  ?竺 ?軸 ?舳 ?忸 ?衄 ?衂)
       ("シチ"  ?七 ?質 ?貭)
       ("シツ"  ?七 ?叱 ?失 ?悉 ?蟋 ?漆 ?膝 ?疾 ?嫉 ?質 ?貭 ?桎 ?室 ?隲 ?櫛 ?瑟 ?執 ?濕 ?湿)
       ("ジツ"  ?日 ?實 ?実)
       ("ヂツ"  ?昵 ?衵)
       ("シャ"  ?這 ?卸 ?叉 ?砂 ?紗 ?沙 ?鯊 ?娑 ?射 ?謝 ?赦 ?斜 ?社 ?者 ?奢 ?偖 ?赭 ?煮 ?車 ?遮 ?蔗 ?鷓 ?寫 ?冩 ?写 ?瀉 ?灑 ?洒 ?炙 ?舍 ?舎 ?捨 ?藉)
       ("ジャ"  ?蛇 ?邪 ?闍 ?麝)
       ("シャク"        ?癪 ?笏 ?勺 ?灼 ?酌 ?妁 ?芍 ?杓 ?爵 ?嚼 ?斫 ?綽 ?鑠 ?爍 ?尺 ?昔 ?借 ?赤 ?釋 ?釈 ?錫)
       ("ジャク"        ?若 ?惹 ?弱 ?雀 ?鵲 ?寂)
       ("ヂャク"        ?著 ?着)
       ("シュ"  ?棕 ?椶 ?種 ?腫 ?從 ?従 ?橦 ?撞 ?主 ?麈 ?取 ?趣 ?諏 ?娵 ?衆 ?朱 ?殊 ?珠 ?侏 ?茱 ?銖 ?株 ?須 ?鬚 ?繻 ?守 ?狩 ?手 ?酒 ?首 ?修)
       ("ジュ"  ?從 ?従 ?从 ?頌 ?樹 ?需 ?儒 ?嬬 ?濡 ?孺 ?襦 ?戍 ?聚 ?受 ?授 ?綬 ?呪 ?咒 ?就 ?壽 ?寿)
       ("シュゥ"        ?終 ?柊 ?螽 ?衆 ?嵩 ?菘 ?宗)
       ("シュウ"        ?聚)
       ("シウ"  ?囚 ?泅 ?周 ?週 ?州 ?洲 ?酬 ?秀 ?銹 ?秋 ?龝 ?穐 ?萩 ?啾 ?楸 ?鞦 ?愁 ?甃 ?湫 ?鍬 ?繍 ?綉 ?臭 ?舟 ?酋 ?遒 ?逎 ?鰌 ?醜 ?蒐 ?祝 ?袖 ?岫 ?鷲 ?就 ?蹴 ?售 ?讎 ?讐 ?收 ?収 ?羞 ?脩 ?修 ?驟)
       ("シフ"  ?執 ?拾 ?習 ?褶 ?襲 ?輯 ?葺 ?緝 ?楫 ?集)
       ("ジュゥ"        ?充 ?銃 ?戎 ?絨 ?從 ?従 ?从 ?縱 ?縦 ?従 ?從)
       ("ジウ"  ?柔 ?蹂 ?鞣 ?獸 ?獣)
       ("ジフ"  ?拾 ?十 ?什 ?汁 ?廿 ?澀 ?渋 ?澁)
       ("ヂュゥ"        ?重)
       ("ヂュウ"        ?住 ?頭)
       ("ヂウ"  ?狃 ?糅)
       ("シュク"        ?叔 ?淑 ?菽 ?俶 ?夙 ?宿 ?縮 ?蓿 ?祝 ?槭 ?蹙 ?倏 ?肅 ?粛)
       ("ジュク"        ?孰 ?塾 ?熟)
       ("シュツ"        ?出 ?蟀)
       ("ジュツ"        ?恤 ?戌 ?朮 ?術 ?述)
       ("シュン"        ?俊 ?峻 ?竣 ?駿 ?悛 ?浚 ?逡 ?春 ?惷 ?蠢 ?舜 ?瞬 ?蕣 ?旬 ?徇 ?洵 ?詢 ?隼 ?濬 ?諄 ?雋 ?儁)
       ("ジュン"        ?閏 ?潤 ?准 ?準 ?凖 ?旬 ?殉 ?徇 ?恂 ?洵 ?筍 ?笋 ?荀 ?詢 ?盾 ?循 ?楯 ?純 ?巡 ?遵 ?順 ?馴 ?隼 ?蓴 ?鶉 ?諄 ?淳 ?醇)
       ("ショ"  ?且 ?砠 ?苴 ?雎 ?杵 ?黍 ?初 ?所 ?庶 ?蔗 ?署 ?薯 ?暑 ?渚 ?緒 ?曙 ?諸 ?藷 ?書 ?恕 ?處 ?処 ?墅 ?野 ?嶼 ?胥 ?蔬)
       ("ジョ"  ?助 ?鋤 ?耡 ?序 ?抒 ?徐 ?敍 ?敘 ?叙 ?汝 ?如 ?洳 ?茹 ?恕 ?絮 ?舒)
       ("ヂョ"  ?女 ?除)
       ("シャゥ"        ?傷 ?殤 ?觴 ?匠 ?商 ?尚 ?嘗 ?甞 ?掌 ?裳 ?賞 ?償 ?敞 ?廠 ?厰 ?昌 ?唱 ?娼 ?菖 ?倡 ?猖 ?晶 ?祥 ?詳 ?庠 ?翔 ?章 ?彰 ?樟 ?障 ?璋 ?嶂 ?瘴 ?象 ?上 ?相 ?湘 ?廂 ?牀 ?床 ?莊 ?荘 ?庄 ?裝 ?装 ?將 ?将 ?蒋 ?醤 ?奬 ?奨 ?漿 ?獎 ?鏘 ?薔 ?檣 ?牆 ?墻 ?餉 ?生 ?笙 ?猩 ?星 ?姓 ?性 ?省 ?井 ?丼 ?正 ?証 ?鉦 ?政 ?聲 ?声 ?青 ?清 ?精)
       ("ショゥ"        ?衝 ?鍾 ?踵 ?訟 ?頌 ?松 ?枩 ?淞 ?鬆 ?從 ?従 ?从 ?慫 ?聳 ?蹤 ?踪 ?悚 ?竦 ?橦 ?鐘 ?憧 ?舂 ?誦 ?勝 ?升 ?昇 ?陞 ?承 ?拯 ?症 ?稱 ?称 ?證 ?証)
       ("(ショゥ)"      ?証)
       ("シャゥ"        ?証)
       ("セイ"  ?証)
       ("セウ"  ?嘯 ?瀟 ?簫 ?蕭 ?召 ?招 ?沼 ?紹 ?詔 ?劭 ?邵 ?韶 ?昭 ?照 ?小 ?少 ?抄 ?炒 ?鈔 ?焦 ?樵 ?礁 ?蕉 ?憔 ?鷦 ?笑 ?咲 ?肖 ?宵 ?消 ?硝 ?峭 ?悄 ?誚 ?逍 ?銷 ?霄 ?鞘 ?哨 ?鮹 ?稍 ?梢 ?愀 ?椒 ?燒 ?焼)
       ("セフ"  ?妾 ?霎 ?捷 ?睫 ?渉 ?囁 ?顳 ?懾 ?燮 ?浹)
       ("ジャゥ"        ?嘗 ?甞 ?常 ?上 ?状 ?奘 ?弉 ?襄 ?驤 ?壤 ?壌 ?攘 ?穰 ?穣 ?禳 ?讓 ?譲 ?情 ?靜 ?静 ?成 ?城 ?盛 ?晟 ?淨 ?浄)
       ("ジョゥ"        ?冗 ?茸 ?丞 ?烝 ?蒸 ?乘 ?乗 ?剩 ?剰 ?仍 ?繩 ?縄)
       ("ゼウ"  ?擾 ?繞 ?蕘 ?遶 ?饒)
       ("ヂャゥ"        ?丈 ?杖 ?仗 ?場 ?塲 ?娘 ?釀 ?醸 ?孃 ?嬢 ?定 ?錠 ?諚)
       ("デウ"  ?嫋 ?嬲 ?條 ?条 ?絛)
       ("デフ"  ?聶 ?躡 ?鑷 ?疉 ?畳 ?疂 ?疊)
       ("ショク"        ?謖 ?稷 ?蜀 ?燭 ?觸 ?触 ?囑 ?嘱 ?矚 ?贖 ?拭 ?軾 ?殖 ?埴 ?植 ?織 ?職 ?色 ?食 ?蝕 ?飾 ?餝 ?喞 ?嗇 ?穡 ?寔 ?昃)
       ("ジョク"        ?辱 ?溽 ?縟 ?蓐 ?褥)
       ("ヂョク"        ?濁)
       ("シム"  ?侵 ?浸 ?寢 ?寝 ?駸 ?審 ?瀋 ?心 ?芯 ?沁 ?森 ?深 ?針 ?岑 ?忱 ?怎 ?斟 ?滲 ?蔘 ?蕈 ?譖 ?譛 ?僣 ?僭 ?簪 ?讖 ?鍼 ?箴)
       ("シン"  ?信 ?新 ?薪 ?親 ?襯 ?申 ?伸 ?紳 ?呻 ?神 ?秦 ?榛 ?臻 ?蓁 ?臣 ?診 ?疹 ?畛 ?袗 ?軫 ?身 ?辛 ?進 ?辰 ?娠 ?振 ?震 ?賑 ?宸 ?晨 ?蜃 ?唇 ?脣 ?津 ?哂 ?晉 ?晋 ?縉 ?眞 ?真 ?愼 ?慎 ?瞋 ?嗔 ?槙 ?齔 ?請)
       ("ジム"  ?壬 ?衽 ?袵 ?荏 ?尋 ?潯 ?蕁 ?甚 ?糂 ?稔)
       ("ジン"  ?神 ?臣 ?腎 ?人 ?儿 ?仁 ?刃 ?刄 ?仞 ?仭 ?靱 ?靭 ?訊 ?迅 ?盡 ?尽 ?儘 ?侭 ?燼 ?贐)
       ("ヂム"  ?沈)
       ("ヂン"  ?塵 ?陣)
       ("ス"    ?子 ?主 ?諏 ?須 ?雛 ?數 ?数 ?素 ?守)
       ("ズ"    ?事 ?亊)
       ("ヅ"    ?廚 ?厨 ?杜 ?圖 ?図 ?豆 ?逗 ?頭 ?荳)
       ("スイ"  ?吹 ?炊 ?垂 ?埀 ?睡 ?捶 ?陲 ?錘 ?惴 ?出 ?帥 ?水 ?翠 ?翆 ?悴 ?忰 ?瘁 ?粹 ?粋 ?醉 ?酔 ?萃 ?膵 ?衰 ?榱 ?遂 ?燧 ?邃 ?隧 ?彗 ?穗 ?穂 ?綏 ?騅 ?錐 ?誰 ?推 ?椎 ?雖)
       ("ズイ"  ?瑞 ?隋 ?髓 ?髄 ?隨 ?随 ?蘂 ?蕊 ?蕋)
       ("スゥ"  ?崇 ?嵩)
       ("スウ"  ?數 ?数 ?樞 ?枢 ?芻 ?蒭 ?雛 ?趨 ?鄒 ?陬)
       ("スン"  ?寸)
       ("セ"    ?施 ?世 ?丗)
       ("ゼ"    ?是)
       ("セィ"  ?生 ?甥 ?牲 ?姓 ?性 ?旌 ?星 ?醒 ?惺 ?腥 ?猩 ?省 ?井 ?丼 ?穽 ?成 ?盛 ?誠 ?晟 ?筬 ?正 ?征 ?政 ?整 ?聖 ?聲 ?声 ?青 ?蜻 ?情 ?晴 ?清 ?精 ?請 ?靖 ?睛 ?菁 ?靜 ?静 ?錆)
       ("セイ"  ?犀 ?凄 ?棲 ?悽 ?淒 ?萋 ?西 ?栖 ?嘶 ?壻 ?婿 ?聟 ?韲 ?齏 ?齊 ?斉 ?齎 ?擠 ?濟 ?済 ?臍 ?躋 ?霽 ?歳 ?世 ?丗 ?制 ?製 ?掣 ?勢 ?逝 ?誓)
       ("ゼイ"  ?脆 ?毳 ?筮 ?噬 ?蛻 ?説 ?税 ?贅)
       ("セキ"  ?責 ?蹟 ?迹 ?磧 ?積 ?績 ?勣 ?隻 ?席 ?蓆 ?蹠 ?斥 ?昔 ?惜 ?籍 ?藉 ?石 ?跖 ?碩 ?脊 ?瘠 ?蹐 ?鶺 ?赤 ?螫 ?跡 ?夕 ?汐 ?寂 ?戚 ?析 ?蜥 ?晰 ?淅 ?皙 ?裼)
       ("セク"  ?齪)
       ("セチ"  ?節)
       ("セツ"  ?説 ?殺 ?刹 ?屑 ?切 ?節 ?截 ?楔 ?竊 ?窃 ?洩 ?絏 ?拙 ?折 ?浙 ?晢 ?設 ?雪 ?泄 ?紲 ?渫 ?緤 ?薛 ?褻 ?接 ?攝 ?摂)
       ("ゼツ"  ?絶 ?舌)
       ("セム"  ?占 ?尖 ?染 ?閃 ?僉 ?簽 ?暹 ?殲 ?殱 ?籤 ?籖 ?孅 ?纖 ?繊 ?纎 ?潛 ?濳 ?潜 ?僭 ?僣 ?蟾 ?瞻 ?贍 ?銛 ?陝 ?芟)
       ("セン"  ?疝 ?仙 ?茜 ?先 ?銑 ?筅 ?跣 ?洗 ?千 ?阡 ?仟 ?薦 ?倩 ?燹 ?荐 ?串 ?蝉 ?嬋 ?戰 ?戦 ?闡 ?宣 ?愃 ?川 ?巛 ?釧 ?扇 ?煽 ?栓 ?詮 ?痊 ?筌 ?銓 ?泉 ?線 ?綫 ?腺 ?煎 ?剪 ?翦 ?箭 ?旋 ?穿 ?羨 ?舛 ?船 ?舩 ?選 ?饌 ?撰 ?鮮 ?癬 ?蘚 ?亘 ?僊 ?遷 ?韆 ?吮 ?專 ?専 ?磚 ?甎 ?尠 ?孱 ?潺 ?旃 ?栴 ?淺 ?浅 ?踐 ?践 ?錢 ?銭 ?賤 ?賎 ?餞 ?濺 ?牋 ?箋 ?鐫 ?顫 ?氈 ?羶 ?擅)
       ("ゼム"  ?漸 ?冉 ?苒 ?髯)
       ("ゼン"  ?前 ?蝉 ?禪 ?禅 ?善 ?譱 ?繕 ?膳 ?然 ?全 ?喘 ?涎 ?蠕 ?漸)
       ("ソ"    ?岨 ?狙 ?咀 ?沮 ?疽 ?齟 ?阻 ?俎 ?爼 ?詛 ?祖 ?租 ?粗 ?組 ?徂 ?疏 ?蔬 ?梳 ?疎 ?楚 ?礎 ?鼠 ?鼡 ?塑 ?愬 ?溯 ?遡 ?措 ?素 ?蘇 ?蘓 ?酥 ?訴 ?泝 ?甦 ?胙 ?祚 ?麁 ?曾 ?曽 ?噌)
       ("サゥ"  ?雙 ?双 ?窗 ?窓 ?粧 ?妝 ?爽 ?相 ?想 ?箱 ?霜 ?孀 ?剏 ?壯 ?壮 ?莊 ?荘 ?庄 ?裝 ?装 ?桑 ?倉 ?蒼 ?滄 ?艙 ?創 ?愴 ?瘡 ?槍 ?搶 ?蹌 ?鎗 ?喪 ?葬 ?爭 ?争 ?崢 ?箏 ?筝 ?諍 ?錚)
       ("サウ"  ?巣 ?樔 ?勦 ?剿 ?爪 ?抓 ?笊 ?操 ?藻 ?燥 ?噪 ?懆 ?澡 ?躁 ?譟 ?早 ?草 ?曹 ?槽 ?漕 ?糟 ?遭 ?蚤 ?掻 ?騷 ?騒 ?嫂 ?艘 ?棗 ?竈 ?竃 ?艸 ?帚 ?箒 ?掃)
       ("サフ"  ?匝 ?卅 ?颯 ?插 ?挿 ?歃)
       ("ソゥ"  ?叢 ?送 ?匆 ?怱 ?葱 ?偬 ?惣 ?總 ?総 ?聰 ?聡 ?宗 ?綜 ?淙 ?粽 ?宋 ?曾 ?曽 ?僧 ?層 ?贈 ?甑)
       ("ソウ"  ?奏 ?湊 ?輳 ?走 ?赱 ?叟 ?搜 ?捜 ?痩 ?溲 ?嗾 ?簇 ?掫 ?漱 ?嗽 ?籔 ?薮 ?藪)
       ("ザゥ"  ?象 ?像 ?藏 ?蔵 ?贓 ?賍 ?臟 ?臓)
       ("ザウ"  ?造 ?慥)
       ("ザフ"  ?雜 ?雑 ?襍)
       ("ゾゥ"  ?増 ?憎 ?贈)
       ("ソク"  ?燭 ?束 ?速 ?足 ?促 ?捉 ?即 ?喞 ?息 ?熄 ?仄 ?塞 ?則 ?側 ?測 ?惻)
       ("ゾク"  ?族 ?蔟 ?鏃 ?粟 ?俗 ?屬 ?属 ?續 ?続 ?賊 ?戝)
       ("ソツ"  ?帥 ?率 ?卒 ?卆 ?猝)
       ("ソン"  ?存 ?拵 ?栫 ?孫 ?遜 ?尊 ?樽 ?鱒 ?蹲 ?損 ?村 ?邨 ?忖 ?巽)
       ("ゾン"  ?存)
       ("タ"    ?太 ?汰 ?他 ?多 ?夛 ?它 ?詑 ?佗 ?唾 ?躱 ?咤 ?岔 ?荼)
       ("ダ"    ?兌 ?舵 ?柁 ?陀 ?沱 ?駝 ?鴕 ?蛇 ?駄 ?騨 ?那 ?娜 ?梛 ?唾 ?妥 ?惰 ?橢 ?楕 ?墮 ?堕 ?懦 ?朶 ?拏 ?拿 ?打)
       ("タイ"  ?替 ?體 ?軆 ?躰 ?太 ?汰 ?泰 ?大 ?帶 ?帯 ?滯 ?滞 ?堆 ?退 ?腿 ?褪 ?隊 ?對 ?対 ?鐓 ?頽 ?耐 ?待 ?態 ?戴 ?逮 ?靆 ?代 ?岱 ?袋 ?貸 ?黛 ?玳 ?怠 ?胎 ?苔 ?殆 ?抬 ?紿 ?詒 ?颱 ?駘 ?臺 ?台 ?擡)
       ("(タイ)"        ?体)
       ("ホン"  ?体)
       ("ダイ"  ?醍 ?題 ?弟 ?第 ?悌 ?大 ?奈 ?内 ?餒 ?代 ?乃 ?臺)
       ("(ダイ)"        ?台)
       ("イ"    ?台)
       ("タク"  ?卓 ?倬 ?濯 ?琢 ?啄 ?托 ?託 ?宅 ?拓 ?度 ?拆 ?柝 ?澤 ?沢 ?擇 ?択 ?鐸 ?磔 ?謫)
       ("ダク"  ?濁 ?諾)
       ("タツ"  ?達 ?逹 ?撻 ?燵 ?闥 ?韃)
       ("ダツ"  ?怛 ?妲 ?獺 ?奪 ?脱)
       ("タム"  ?堪 ?湛 ?探 ?耽 ?眈 ?壜 ?罎 ?覃 ?潭 ?譚 ?鐔 ?淡 ?啖 ?毯 ?痰 ?餤 ?啗 ?擔 ?担 ?憺 ?檐 ?澹 ?膽 ?胆 ?站)
       ("タン"  ?反 ?丹 ?旦 ?但 ?坦 ?疸 ?靼 ?亶 ?壇 ?檀 ?袒 ?歎 ?嘆 ?攤 ?炭 ?蛋 ?誕 ?蜑 ?單 ?単 ?憚 ?箪 ?殫 ?襌 ?褝 ?鄲 ?短 ?鍛 ?緞 ?彖 ?湍 ?端 ?赧 ?綻)
       ("ダム"  ?男 ?談)
       ("ダン"  ?壇 ?檀 ?彈 ?弾 ?暖 ?煖 ?段 ?椴 ?團 ?団 ?斷 ?断)
       ("チ"    ?知 ?智 ?蜘 ?踟 ?雉 ?池 ?馳 ?弛 ?地 ?褫 ?魑 ?黐 ?稚 ?胝 ?躓 ?輊 ?致 ?緻 ?遲 ?遅 ?穉 ?黹 ?治 ?値 ?置 ?恥 ?耻 ?癡 ?痴)
       ("チク"  ?畜 ?蓄 ?竹 ?竺 ?筑 ?築 ?逐 ?矗)
       ("チツ"  ?秩 ?帙 ?窒 ?膣 ?腟 ?蟄)
       ("チャ"  ?茶)
       ("チャク"        ?著 ?着 ?嫡)
       ("チュゥ"        ?中 ?沖 ?冲 ?仲 ?忠 ?衷 ?蟲)
       ("(チュゥ)"      ?虫)
       ("クヰ"  ?虫)
       ("チュウ"        ?注 ?註 ?柱 ?駐 ?廚 ?厨 ?誅 ?鍮 ?偸)
       ("チウ"  ?丑 ?紐 ?鈕 ?宙 ?抽 ?紬 ?冑 ?胄 ?肘 ?紂 ?酎 ?儔 ?疇 ?畴 ?籌 ?躊 ?鑄 ?鋳 ?晝 ?昼 ?籀 ?綢 ?惆 ?稠)
       ("チュツ"        ?黜)
       ("チュン"        ?椿)
       ("チョ"  ?緒 ?儲 ?楮 ?豬 ?瀦 ?猪 ?潴 ?著 ?躇 ?樗 ?貯 ?苧 ?竚 ?紵 ?佇 ?杼)
       ("チャゥ"        ?暢 ?腸 ?膓 ?長 ?帳 ?脹 ?悵 ?萇 ?張 ?漲 ?昶 ?鬯 ?丁 ?町 ?甼 ?頂 ?訂 ?疔 ?聽 ?听 ?聴 ?廳 ?廰 ?庁)
       ("チョゥ"        ?重 ?寵 ?冢 ?塚 ?澄 ?徴 ?懲 ?澂)
       ("テウ"  ?鯛 ?凋 ?調 ?雕 ?彫 ?弔 ?吊 ?鳥 ?蔦 ?釣 ?髫 ?迢 ?齠 ?超 ?兆 ?晁 ?眺 ?跳 ?銚 ?佻 ?窕 ?誂 ?挑 ?朝 ?潮 ?嘲 ?肇 ?趙)
       ("テフ"  ?輒 ?輙 ?喋 ?牒 ?蝶 ?諜 ?貼 ?帖)
       ("チョク"        ?躅 ?捗 ?陟 ?直 ?飭 ?敕 ?勅)
       ("チム"  ?砧 ?碪 ?朕 ?沈 ?鴆 ?酖 ?枕 ?賃 ?椹 ?闖)
       ("チン"  ?珍 ?珎 ?趁 ?陳 ?鎭 ?填 ?鎮 ?椿 ?亭)
       ("ツ"    ?都)
       ("ツイ"  ?墜 ?追 ?槌 ?鎚 ?堆 ?椎 ?對 ?対)
       ("ツゥ"  ?痛 ?通)
       ("デ"    ?弟)
       ("ティ"  ?貞 ?偵 ?遉 ?禎 ?幀 ?呈 ?程 ?逞 ?酲 ?鄭 ?蟶 ?騁 ?丁 ?汀 ?訂 ?釘 ?叮 ?酊 ?亭 ?停 ?渟 ?定 ?碇 ?廷 ?庭 ?挺 ?梃 ?艇 ?霆 ?鼎)
       ("テイ"  ?堤 ?提 ?帝 ?締 ?諦 ?蹄 ?啼 ?蒂 ?底 ?低 ?抵 ?邸 ?柢 ?羝 ?觝 ?詆 ?牴 ?弟 ?剃 ?悌 ?梯 ?涕 ?睇 ?嚔 ?嚏 ?棣 ?遞 ?逓 ?體 ?体 ?軆 ?躰 ?髢 ?綴)
       ("デイ"  ?泥)
       ("テキ"  ?擢 ?糴 ?擲 ?躑 ?摘 ?敵 ?鏑 ?滴 ?適 ?的 ?笛 ?廸 ?迪 ?剔 ?狄 ?荻 ?逖 ?覿)
       ("デキ"  ?溺)
       ("テツ"  ?迭 ?跌 ?姪 ?咥 ?垤 ?耋 ?鐵 ?鉄 ?銕 ?鐡 ?綴 ?輟 ?錣 ?哲 ?徹 ?轍 ?撤 ?屮)
       ("テム"  ?諂 ?店 ?點 ?点 ?貼 ?沾 ?覘 ?霑 ?甜 ?忝 ?添 ?恬 ?簟)
       ("テン"  ?典 ?腆 ?填 ?槙 ?顛 ?巓 ?癲 ?天 ?殿 ?奠 ?畋 ?殄 ?靦 ?展 ?碾 ?輾 ?廛 ?厘 ?纏 ?纒 ?躔 ?椽 ?篆 ?轉 ?転)
       ("デム"  ?鮎)
       ("デン"  ?殿 ?澱 ?癜 ?臀 ?田 ?佃 ?甸 ?鈿 ?電 ?傳 ?伝)
       ("ト"    ?堵 ?屠 ?賭 ?都 ?睹 ?覩 ?塗 ?途 ?荼 ?妬 ?徒 ?跿 ?度 ?渡 ?鍍 ?土 ?吐 ?杜 ?肚 ?兔 ?兎 ?菟 ?圖 ?図 ?蠹 ?蠧 ?登 ?兜 ?斗 ?蚪 ?頭)
       ("ド"    ?度 ?土 ?奴 ?努 ?怒 ?孥 ?弩 ?駑 ?帑 ?呶)
       ("タゥ"  ?唐 ?塘 ?糖 ?宕 ?湯 ?蕩 ?盪 ?棠 ?黨 ?党 ?儻 ?螳 ?當 ?当 ?礑 ?蟷 ?襠 ?鐺 ?档 ?橙)
       ("タウ"  ?啅 ?棹 ?罩 ?悼 ?掉 ?櫂 ?刀 ?叨 ?到 ?倒 ?套 ?島 ?嶋 ?嶌 ?搗 ?桃 ?逃 ?迯 ?討 ?陶 ?淘 ?掏 ?綯 ?道 ?檮 ?梼 ?濤 ?涛 ?祷 ?擣 ?盜 ?盗 ?稻 ?稲 ?滔 ?蹈 ?韜 ?纛 ?饕)
       ("タフ"  ?沓 ?踏 ?鞜 ?答 ?荅 ?鞳 ?搭 ?塔 ?納 ?搨 ?榻 ?譫)
       ("トゥ"  ?桶 ?桐 ?筒 ?東 ?凍 ?棟 ?董 ?瞳 ?橦 ?潼 ?幢 ?冬 ?疼 ?苳 ?鼕 ?統 ?登 ?燈 ?灯 ?鐙 ?嶝 ?磴 ?等 ?謄 ?騰 ?縢 ?籘 ?滕 ?藤 ?籐)
       ("トウ"  ?透 ?兜 ?投 ?骰 ?豆 ?逗 ?痘 ?頭 ?偸 ?抖 ?竇 ?讀 ?読 ?鬪 ?闘 ?鬥)
       ("ダゥ"  ?堂 ?螳 ?瞠 ?曩 ?獰)
       ("ダウ"  ?撓 ?橈 ?鐃 ?閙 ?鬧 ?萄 ?道 ?導)
       ("ダフ"  ?衲)
       ("ドゥ"  ?動 ?働 ?仂 ?慟 ?同 ?仝 ?桐 ?洞 ?胴 ?銅 ?恫 ?童 ?瞳 ?僮 ?撞 ?艟 ?憧 ?藤)
       ("ドウ"  ?耨)
       ("トク"  ?涜 ?牘 ?犢 ?黷 ?讀 ?読 ?禿 ?督 ?篤 ?匿 ?慝 ?得 ?徳 ?悳 ?特)
       ("ドク"  ?獨 ?独 ?髑 ?讀 ?読 ?毒)
       ("トツ"  ?凸 ?突 ?吶 ?訥 ?咄)
       ("ドツ"  ?肭)
       ("トン"  ?屯 ?沌 ?飩 ?頓 ?惇 ?敦 ?暾 ?燉 ?遁 ?遯 ?團 ?団)
       ("ドム"  ?曇 ?壜 ?罎 ?貪)
       ("ドン"  ?鈍 ?飩 ?嫩 ?呑 ?緞)
       ("ナ"    ?奈 ?那 ?儺 ?納)
       ("ナイ"  ?内 ?乃 ?迺)
       ("ナツ"  ?捺)
       ("ナム"  ?男 ?南 ?楠 ?喃 ?納)
       ("ナン"  ?難 ?軟)
       ("ニ"    ?爾 ?尓 ?兒 ?児 ?邇 ?迩 ?二 ?弍 ?貳 ?貮 ?弐 ?仁 ?尼)
       ("ニク"  ?肉 ?宍)
       ("ニチ"  ?日)
       ("ニャク"        ?若 ?蒻)
       ("ニュウ"        ?乳)
       ("ニウ"  ?柔)
       ("ニフ"  ?廿 ?入)
       ("ニョ"  ?女 ?如)
       ("ニョウ"        ?女)
       ("ネウ"  ?尿 ?繞 ?遶 ?鐃)
       ("ニム"  ?壬 ?妊 ?姙 ?任)
       ("ニン"  ?人 ?儿 ?仁 ?忍 ?認 ?荵)
       ("ネ"    ?禰 ?祢 ?涅)
       ("ネィ"  ?寧 ?嚀 ?濘 ?檸 ?佞 ?侫)
       ("ネツ"  ?捏 ?熱)
       ("ネム"  ?鮎 ?拈 ?粘 ?黏 ?念 ?捻 ?唸 ?稔)
       ("ネン"  ?年 ?然 ?燃 ?撚)
       ("ナゥ"  ?曩 ?嚢)
       ("ナウ"  ?惱 ?悩 ?瑙 ?碯 ?腦 ?脳)
       ("ナフ"  ?納)
       ("ノゥ"  ?農 ?膿 ?濃 ?能)
       ("ハ"    ?派 ?叭 ?播 ?陂 ?破 ?簸 ?頗 ?坡 ?玻 ?波 ?菠 ?跛 ?巴 ?把 ?杷 ?琶 ?爬 ?笆 ?耙 ?葩 ?霸 ?覇)
       ("バ"    ?婆 ?芭 ?馬 ?罵 ?蟆 ?蟇)
       ("ハイ"  ?旆 ?沛 ?霈 ?肺 ?稗 ?牌 ?擺 ?俳 ?排 ?輩 ?徘 ?琲 ?裴 ?憊 ?拜 ?拝 ?湃 ?唄 ?敗 ?背 ?配 ?佩 ?珮 ?坏 ?杯 ?盃 ?悖 ?胚 ?廢 ?廃 ?癈)
       ("バイ"  ?眛 ?貝 ?狽 ?唄 ?買 ?賣 ?売 ?霾 ?黴 ?倍 ?賠 ?陪 ?焙 ?培 ?媒 ?煤 ?梅 ?楳 ?莓 ?吠)
       ("ハク"  ?剥 ?璞 ?駮 ?薄 ?博 ?搏 ?膊 ?亳 ?白 ?拍 ?柏 ?栢 ?舶 ?迫 ?魄 ?伯 ?珀 ?泊 ?畠 ?粕 ?箔 ?狛 ?佰 ?陌 ?帛 ?擘 ?檗 ?蘗)
       ("バク"  ?暴 ?曝 ?瀑 ?爆 ?駁 ?藐 ?博 ?縛 ?莫 ?漠 ?寞 ?獏 ?貘 ?驀 ?幕 ?貊 ?麥 ?麦)
       ("ハチ"  ?鉢 ?八)
       ("バチ"  ?罰 ?罸 ?撥)
       ("ハツ"  ?發 ?発 ?溌 ?撥 ?醗 ?髮 ?髪 ?鉢)
       ("バツ"  ?伐 ?筏 ?閥 ?罰 ?罸 ?末 ?拔 ?抜 ?跋 ?魃)
       ("ハム"  ?犯 ?氾 ?范 ?笵 ?範 ?凡 ?帆 ?汎 ?泛)
       ("ハン"  ?反 ?販 ?飯 ?阪 ?坂 ?叛 ?板 ?鈑 ?版 ?煩 ?樊 ?礬 ?攀 ?繁 ?半 ?伴 ?判 ?畔 ?拌 ?絆 ?胖 ?袢 ?般 ?搬 ?槃 ?瘢 ?斑 ?班 ?頒 ?釆 ?旛 ?旙 ?燔 ?繙 ?膰 ?鷭 ?藩 ?蟠 ?潘)
       ("バン"  ?挽 ?晩 ?輓 ?悗 ?番 ?蕃 ?卍 ?萬 ?万 ?伴 ?判 ?絆 ?盤 ?磐 ?板 ?鈑 ?蠻 ?蛮)
       ("ヒ"    ?匙 ?卑 ?碑 ?俾 ?婢 ?脾 ?裨 ?髀 ?痺 ?皮 ?彼 ?披 ?疲 ?被 ?陂 ?鞁 ?罷 ?臂 ?譬 ?避 ?ヒ ?否 ?痞 ?比 ?庇 ?妣 ?屁 ?秕 ?粃 ?豼 ?貔 ?紕 ?批 ?砒 ?蓖 ?泌 ?祕 ?秘 ?丕 ?嚊 ?贔 ?鄙 ?鞴 ?妃 ?肥 ?費 ?狒 ?非 ?扉 ?斐 ?緋 ?誹 ?翡 ?腓 ?菲 ?蜚 ?霏 ?匪 ?榧 ?悲 ?飛)
       ("ビ"    ?弭 ?彌 ?弥 ?瀰 ?糜 ?縻 ?靡 ?備 ?糒 ?枇 ?琵 ?毘 ?眉 ?媚 ?嵋 ?美 ?鼻 ?寐 ?麋 ?尾 ?微 ?薇)
       ("ヒチ"  ?篳)
       ("ヒツ"  ?匹 ?疋 ?弼 ?必 ?泌 ?謐 ?畢 ?蹕 ?筆 ?逼)
       ("ヒャク"        ?百)
       ("ビャク"        ?白 ?闢)
       ("ヒウ"  ?彪)
       ("ビウ"  ?謬 ?繆)
       ("ヒャゥ"        ?評 ?兵)
       ("ヒャウ"        ?拍)
       ("ヒョゥ"        ?冰 ?冫 ?氷 ?馮 ?憑)
       ("ヒョウ"        ?雹)
       ("ヘウ"  ?票 ?標 ?漂 ?瓢 ?剽 ?嫖 ?慓 ?縹 ?飄 ?飃 ?驃 ?鰾 ?表 ?俵 ?殍 ?飆 ?髟 ?豹 ?彪)
       ("ビャゥ"        ?病 ?平 ?屏)
       ("ベウ"  ?廟 ?秒 ?杪 ?眇 ?渺 ?緲 ?苗 ?描 ?錨 ?猫)
       ("ヒム"  ?品)
       ("ヒン"  ?彬 ?斌 ?貧 ?賓 ?擯 ?檳 ?梹 ?殯 ?濱 ?嬪 ?繽 ?頻 ?瀕 ?蘋 ?顰 ?牝)
       ("(ヒン)"        ?浜)
       ("ハウ"  ?浜)
       ("ヒャウ"        ?浜)
       ("ビン"  ?貧 ?敏 ?岷 ?愍 ?泯 ?緡 ?旻 ?閔 ?憫 ?紊 ?檳 ?梹 ?鬢 ?黽 ?便 ?瓶)
       ("フ"    ?風 ?付 ?符 ?附 ?坿 ?拊 ?柎 ?苻 ?府 ?腐 ?俯 ?腑 ?夫 ?扶 ?芙 ?趺 ?麩 ?麸 ?父 ?斧 ?釜 ?釡 ?膚 ?賦 ?赴 ?訃 ?仆 ?甫 ?敷 ?輔 ?黼 ?傅 ?榑 ?賻 ?溥 ?俛 ?孚 ?郛 ?俘 ?孵 ?桴 ?浮 ?艀 ?蜉 ?巫 ?布 ?怖 ?普 ?譜 ?歩 ?缶 ?不 ?婦 ?富 ?冨 ?負 ?阜 ?埠)
       ("ブ"    ?奉 ?侮 ?武 ?無 ?撫 ?舞 ?蕪 ?嘸 ?廡 ?憮 ?无 ?誣 ?部 ?葡 ?歩 ?分)
       ("フゥ"  ?風 ?楓 ?瘋 ?諷 ?封)
       ("フウ"  ?夫 ?富 ?冨)
       ("フク"  ?伏 ?袱 ?副 ?蝠 ?匐 ?幅 ?福 ?輻 ?蔔 ?服 ?箙 ?蝮 ?腹 ?複 ?馥 ?復 ?覆 ?愎)
       ("ブク"  ?茯)
       ("フツ"  ?不 ?弗 ?佛 ?仏 ?彿 ?拂 ?払 ?髴 ?怫 ?沸 ?祓 ?黻)
       ("ブツ"  ?物 ?佛 ?仏)
       ("フン"  ?分 ?粉 ?紛 ?雰 ?吩 ?忿 ?枌 ?氛 ?汾 ?芬 ?扮 ?吻 ?刎 ?焚 ?奮 ?糞 ?賁 ?憤 ?墳 ?濆 ?噴 ?吩)
       ("ブン"  ?分 ?文 ?攵 ?紊 ?聞)
       ("ヘィ"  ?丙 ?病 ?柄 ?炳 ?兵 ?平 ?苹 ?萍 ?秉 ?并 ?併 ?餠 ?餅 ?屏 ?聘 ?娉 ?竝 ?並)
       ("ヘイ"  ?閉 ?閇 ?陛 ?篦 ?箆 ?俾 ?睥 ?嬖 ?薜 ?敝 ?幣 ?幤 ?蔽 ?弊 ?斃 ?塀)
       ("ベイ"  ?米 ?袂)
       ("ヘキ"  ?碧 ?辟 ?璧 ?襞 ?躄 ?闢 ?僻 ?甓 ?壁 ?劈 ?霹 ?癖)
       ("ベキ"  ?冪 ?羃 ?幎 ?汨 ?覓)
       ("ベツ"  ?蔑 ?韈 ?襪 ?別 ?瞥 ?鼈)
       ("ヘム"  ?貶)
       ("ヘン"  ?返 ?扁 ?遍 ?蝙 ?編 ?偏 ?篇 ?翩 ?褊 ?諞 ?騙 ?片 ?胼 ?駢 ?邊 ?辺 ?邉 ?變 ?変)
       ("ベン"  ?眄 ?駢 ?便 ?鞭 ?勉 ?娩 ?俛 ?冕 ?弁 ?辨 ?辧 ?辯 ?瓣 ?辮 ?卞 ?抃)
       ("ホ"    ?甫 ?輔 ?脯 ?黼 ?鋪 ?舖 ?舗 ?圃 ?捕 ?補 ?哺 ?逋 ?餔 ?浦 ?蒲 ?匍 ?歩 ?保 ?堡 ?葆)
       ("ボ"    ?募 ?墓 ?慕 ?暮 ?模 ?謨 ?簿 ?菩 ?牡 ?戊 ?母 ?拇 ?姆)
       ("ハゥ"  ?邦 ?訪 ?髣 ?魴 ?放 ?倣 ?芳 ?枋 ?彷 ?舫 ?磅 ?彭 ?澎 ?幇 ?烹 ?亨 ?棚 ?繃 ?弸 ?硼 ?萠 ?萌 ?萌 ?怦)
       ("ハゥ（方向・方法の意の場合）" ?方)
       ("ハウ"  ?判 ?包 ?庖 ?砲 ?胞 ?飽 ?咆 ?枹 ?炮 ?皰 ?疱 ?苞 ?靤 ?髱 ?鮑 ?蚫 ?麭 ?匏 ?泡 ?抛)
       ("ハフ"  ?琺)
       ("ハフ（漢音。一般語の場合）"    ?法)
       ("ホゥ"  ?鳳 ?封 ?奉 ?俸 ?捧 ?豐 ?豊 ?峯 ?峰 ?蜂 ?鋒 ?烽 ?逢 ?縫 ?蓬 ?篷 ?朋 ?崩 ?鵬 ?堋)
       ("ホゥ（四角の意の場合）" ?方)
       ("ホウ"  ?焙 ?鴇 ?呆 ?襃 ?褒 ?堡 ?葆 ?褓 ?報 ?抱 ?袍 ?寶 ?寳 ?宝 ?抔)
       ("ホフ"  ?琺)
       ("ホフ（呉音。仏教語に用ゐる）"  ?法)
       ("バゥ"  ?尨 ?厖 ?蚌 ?亡 ?忘 ?望 ?妄 ?芒 ?鋩 ?惘 ?氓 ?忙 ?茫 ?坊 ?妨 ?紡 ?肪 ?防 ?房 ?旁 ?傍 ?滂 ?膀 ?蒡 ?謗 ?榜 ?蠎 ?蟒 ?膨 ?萌 ?黽)
       ("バウ"  ?卯 ?夘 ?茆 ?昴 ?茅 ?貌 ?皃)
       ("ボゥ"  ?蚌 ?棒 ?儚)
       ("ボウ"  ?呆 ?暴 ?冒 ?帽 ?瑁 ?旄 ?髦 ?鉾 ?眸 ?鴾 ?剖 ?某 ?謀 ?貿 ?袤 ?楙 ?懋)
       ("ボフ"  ?乏)
       ("ホク"  ?北)
       ("ボク"  ?卜 ?朴 ?牧 ?睦 ?穆 ?木 ?目 ?樸 ?撲 ?蹼 ?僕 ?濮 ?鶩 ?墨)
       ("ボツ"  ?孛 ?勃 ?渤 ?沒 ?没 ?歿)
       ("ホン"  ?反 ?繙 ?翻 ?飜 ?奔 ?本 ?夲 ?笨)
       ("ボム"  ?凡 ?梵)
       ("ボン"  ?煩 ?盆)
       ("マ"    ?麻 ?痲 ?痳 ?嘛 ?摩 ?磨 ?魔 ?麼)
       ("マイ"  ?米 ?埋 ?邁 ?妹 ?昧 ?眛 ?枚 ?毎 ?瑁)
       ("マク"  ?膜 ?幕)
       ("マツ"  ?末 ?抹 ?沫 ?秣 ?茉 ?靺)
       ("マン"  ?卍 ?萬 ?万 ?曼 ?鰻 ?漫 ?幔 ?鏝 ?饅 ?慢 ?鬘 ?蔓 ?縵 ?謾 ?瞞 ?蹣 ?滿 ?満 ?懣)
       ("ミ"    ?彌 ?弥 ?靡 ?眉 ?未 ?味 ?魅)
       ("ミツ"  ?密 ?樒 ?櫁 ?蜜)
       ("ミャク"        ?脈 ?脉)
       ("ミャゥ"        ?命 ?明 ?名 ?茗 ?冥)
       ("メウ"  ?妙)
       ("ミン"  ?民 ?眠 ?明)
       ("ム"    ?夢 ?梦 ?武 ?鵡 ?無 ?无 ?毋 ?謀 ?牟 ?矛 ?務 ?霧)
       ("メ"    ?瑪 ?碼)
       ("メィ"  ?命 ?明 ?盟 ?鳴 ?名 ?銘 ?茗 ?酩 ?冥 ?暝 ?溟 ?螟 ?瞑)
       ("メイ"  ?迷)
       ("メツ"  ?滅)
       ("メン"  ?麪 ?麺 ?免 ?棉 ?綿 ?緜 ?面 ?緬 ?湎)
       ("モ"    ?摸 ?模 ?茂)
       ("マゥ"  ?亡 ?望 ?妄 ?罔 ?網 ?魍 ?盲 ?孟 ?猛 ?莽)
       ("マウ"  ?耗)
       ("モゥ"  ?蒙 ?曚 ?朦 ?濛 ?矇 ?艨)
       ("モウ"  ?毛 ?耄)
       ("モク"  ?睦 ?木 ?沐 ?杢 ?目 ?苜 ?默 ?黙)
       ("モチ"  ?勿)
       ("モツ"  ?物)
       ("モン"  ?文 ?攵 ?紋 ?門 ?悶 ?們 ?捫 ?聞 ?問)
       ("ヤ"    ?也 ?冶 ?夜 ?耶 ?爺 ?揶 ?椰 ?野 ?埜)
       ("ヤク"  ?約 ?葯 ?躍 ?藥 ?葯 ?薬 ?龠 ?籥 ?鑰 ?厄 ?扼 ?阨 ?疫 ?役 ?益 ?亦 ?譯 ?訳)
       ("ユ"    ?兪 ?瘉 ?愈 ?癒 ?喩 ?楡 ?渝 ?瑜 ?蝓 ?覦 ?逾 ?踰 ?諭 ?愉 ?揄 ?輸 ?臾 ?腴 ?諛 ?萸 ?由 ?油 ?柚 ?遊)
       ("ユイ"  ?惟 ?唯 ?遺)
       ("イウ"  ?右 ?佑 ?祐 ?酉 ?楢 ?猷 ?猶 ?犹 ?蕕 ?又 ?有 ?友 ?尤 ?疣 ?肬 ?憂 ?優 ?有 ?侑 ?宥 ?囿 ?由 ?蚰 ?釉 ?鼬 ?柚 ?誘 ?莠 ?遊 ?游 ?蝣 ?郵 ?攸 ?悠 ?幽 ?黝)
       ("イフ"  ?揖 ?邑 ?悒)
       ("ユゥ"  ?熊 ?雄 ?融 ?涌 ?勇 ?湧)
       ("ユウ"  ?裕)
       ("ヨ"    ?予 ?豫 ?預 ?蕷 ?余 ?餘 ?畭 ?蜍 ?淤 ?舁 ?輿 ?與 ?与 ?歟 ?譽 ?誉 ?飫)
       ("エウ"  ?幺 ?窈 ?幼 ?拗 ?杳 ?窯 ?窰 ?要 ?腰 ?夭 ?妖 ?殀 ?姚 ?搖 ?揺 ?謠 ?謡 ?鷂 ?遥 ?徭 ?瑶 ?窰 ?燿 ?曜 ?耀 ?邀)
       ("エフ"  ?葉 ?頁 ?曄 ?靨)
       ("ヤゥ"  ?羊 ?洋 ?佯 ?痒 ?樣 ?様 ?漾 ?養 ?瀁 ?癢 ?昜 ?揚 ?楊 ?暘 ?煬 ?陽 ?瘍 ?瓔 ?珱)
       ("ヨゥ"  ?容 ?溶 ?熔 ?蓉 ?鎔 ?用 ?庸 ?傭 ?慵 ?廱 ?癰 ?甬 ?踊 ?慂 ?蛹 ?湧 ?踴 ?俑 ?雍 ?擁 ?壅 ?鷹 ?膺)
       ("ヨク"  ?沃 ?浴 ?峪 ?欲 ?慾 ?抑 ?翌 ?翊 ?翼 ?弋 ?杙)
       ("ラ"    ?羅 ?蘿 ?邏 ?鑼 ?螺 ?騾 ?裸)
       ("ライ"  ?禮 ?礼 ?頼 ?瀬 ?癩 ?籟 ?雷 ?擂 ?儡 ?櫑 ?罍 ?磊 ?來 ?来 ?徠 ?賚 ?莱)
       ("ラク"  ?犖 ?絡 ?酪 ?珞 ?烙 ?駱 ?洛 ?落 ?樂 ?楽 ?擽)
       ("ラチ"  ?埒 ?埓)
       ("ラツ"  ?剌 ?辣 ?喇 ?拉)
       ("ラム"  ?嵐 ?婪 ?濫 ?藍 ?籃 ?襤 ?覽 ?覧 ?攬 ?欖 ?纜)
       ("ラン"  ?懶 ?嬾 ?闌 ?欄 ?瀾 ?爛 ?蘭 ?襴 ?卵 ?亂 ?乱 ?欒 ?鑾 ?巒 ?鸞)
       ("リ"    ?璃 ?漓 ?離 ?罹 ?詈 ?驪 ?利 ?梨 ?痢 ?俐 ?犂 ?犁 ?蜊 ?莉 ?履 ?莅 ?吏 ?李 ?里 ?浬 ?鯉 ?狸 ?哩 ?理 ?裏 ?裡 ?俚 ?貍 ?釐)
       ("リキ"  ?力)
       ("リク"  ?陸 ?勠 ?戮)
       ("リチ"  ?律)
       ("リツ"  ?栗 ?慄 ?篥 ?律 ?葎 ?率 ?立)
       ("リャク"        ?掠 ?略 ?畧)
       ("リュゥ"        ?隆 ?窿 ?龍 ?竜)
       ("リウ"  ?柳 ?留 ?畄 ?溜 ?澑 ?瑠 ?琉 ?璢 ?榴 ?瘤 ?霤 ?餾 ?劉 ?嚠 ?瀏 ?流 ?硫 ?旒 ?鏐)
       ("リフ"  ?立 ?笠 ?苙 ?粒)
       ("リョ"  ?慮 ?鑢 ?虜 ?旅 ?膂 ?呂 ?侶 ?梠 ?閭)
       ("リャゥ"        ?椋 ?涼 ?凉 ?諒 ?亮 ?喨 ?良 ?量 ?糧 ?粮 ?兩 ?両 ?倆 ?裲 ?輛 ?輌 ?魎 ?粱 ?梁 ?怜 ?玲 ?領 ?嶺 ?靈 ?霊)
       ("リョゥ"        ?綾 ?菱 ?蔆 ?凌 ?崚 ?陵 ?稜)
       ("レウ"  ?了 ?寮 ?僚 ?遼 ?瞭 ?撩 ?鷯 ?鐐 ?繚 ?療 ?燎 ?料 ?寥 ?廖 ?聊)
       ("レフ"  ?漁 ?獵 ?猟)
       ("リョク"        ?緑 ?力)
       ("リム"  ?林 ?霖 ?淋 ?琳 ?痳 ?醂 ?臨 ?稟 ?禀 ?凛 ?廩 ?懍)
       ("リン"  ?釐 ?厘 ?燐 ?鱗 ?麟 ?鄰 ?隣 ?吝 ?悋 ?藺 ?躪 ?躙 ?倫 ?輪 ?淪 ?綸 ?鈴)
       ("ル"    ?流 ?留 ?畄 ?瑠 ?璢 ?瘻 ?縷 ?僂 ?褸 ?鏤 ?屡)
       ("ルイ"  ?羸 ?涙 ?泪 ?類 ?壘 ?塁 ?縲 ?累 ?瘰 ?誄)
       ("レィ"  ?令 ?嶺 ?冷 ?伶 ?怜 ?玲 ?苓 ?鈴 ?囹 ?羚 ?聆 ?蛉 ?鴒 ?齡 ?齢 ?零 ?澪 ?靈 ?霊 ?櫺)
       ("レイ"  ?戻 ?唳 ?捩 ?綟 ?麗 ?儷 ?禮 ?礼 ?醴 ?茘 ?蠡 ?隸 ?隷 ?黎 ?藜 ?例 ?癘 ?糲 ?勵 ?励 ?礪 ?砺 ?蠣 ?蛎)
       ("レキ"  ?歴 ?暦 ?櫪 ?瀝 ?癧 ?轣 ?靂 ?轢 ?櫟 ?檪 ?礫)
       ("レツ"  ?列 ?烈 ?裂 ?冽 ?洌 ?劣)
       ("レム"  ?鎌 ?廉 ?簾 ?濂 ?匳 ?奩 ?斂 ?瀲)
       ("レン"  ?憐 ?楝 ?煉 ?錬 ?練 ?聯 ?聨 ?連 ?漣 ?鏈 ?蓮 ?戀 ?恋 ?攣 ?臠 ?輦)
       ("ロ"    ?呂 ?絽 ?櫚 ?魯 ?櫓 ?艪 ?賂 ?輅 ?路 ?鷺 ?蕗 ?露 ?盧 ?櫨 ?瀘 ?爐 ?炉 ?艫 ?蘆 ?芦 ?轤 ?鑪 ?鈩 ?顱 ?濾 ?廬 ?臚 ?驢 ?鹵 ?髏)
       ("ラゥ"  ?瀧 ?糧 ?粮 ?朗 ?朖 ?浪 ?狼 ?琅 ?瑯 ?郎 ?廊 ?榔 ?螂 ?踉)
       ("ラウ"  ?牢 ?老 ?勞 ?労 ?撈 ?癆 ?潦 ?醪)
       ("ラフ"  ?臘 ?臈 ?蝋 ?鑞)
       ("ロゥ"  ?弄 ?哢 ?聾 ?朧 ?瓏 ?蘢 ?隴 ?槞 ?籠 ?篭 ?稜 ?薐 ?楞)
       ("ロウ"  ?婁 ?螻 ?樓 ?楼 ?簍 ?鏤 ?髏 ?漏 ?陋)
       ("ロク"  ?鹿 ?麓 ?漉 ?轆 ?六 ?緑 ?録 ?碌 ?祿 ?禄 ?勒 ?肋)
       ("ロン"  ?論 ?崙 ?崘)
       ("ワ"    ?話 ?倭 ?和 ?萵 ?窪 ?凹)
       ("ワイ"  ?歪 ?矮 ?賄 ?猥 ?隈 ?穢)
       ("ワク"  ?惑)
       ("ワン"  ?椀 ?碗 ?腕 ?弯 ?彎 ?灣 ?湾 ?綰)))
    table))

(defconst seikana-menu-keys '(?a ?s ?d ?f ?j ?k ?l)
  "複數の康煕字典體漢字の候補があったときの選擇につかふキー")

(defun seikana-select (cs)
  "複數候補選擇用の下請け關數"
  (cond
   ((null cs)
    nil)
   ((> (length cs) (length seikana-menu-keys))
    (error "internal error. candidates list too long."))
   (t
    (let* ((alis (cl-mapcar #'cons seikana-menu-keys cs))
           (rs
            (cl-mapcar (lambda (k c)
                         (format "%c:%s\(%s\)  " k (car c) (cdr c)))
                       seikana-menu-keys cs))
           (msg
            (apply #'concat (append rs '("n: not replace ")))))
      (catch 'break
        (while t
          (message msg)
          (let* ((c (read-char))
                 (res (assoc c alis)))
            (cond (res
                   (throw 'break (cadr res)))
                  ((char-equal c ?n)
                   (throw 'break nil))
                  (t
                   (message "`%c' is not valid here." c))))))))))

(defun seikana-display-ziom-for-char-at-point ()
  "カーソル位置の漢字の字音を表示する"
  (interactive)
  (let* ((ch (char-before))
         (ziom (gethash ch seikana-kanzi-ziom-table ())))
    (if (null ziom)
        (message "can't find ziom for `%c'" ch)
      (message "%s" ziom))))

(defun seikana-ryakuzi-region (beg end do-force)
  "領域内の康煕字典體の漢字を新字體の漢字におきかへる
前置引數を指定すると置換時に確認をしない"
  (interactive "*r\nP")
  (seikana-replace seikana-seizi-pat
                   seikana-seizi-ryakuzi-table
                   beg end
                   do-force))

(defun seikana-seizi-region (beg end do-force)
  "領域内の新字體の漢字を康煕字典體の漢字におきかへる
前置引數を指定すると置換時に確認をしない"
  (interactive "*r\nP")
  (seikana-replace seikana-ryakuzi-pat
                   seikana-ryakuzi-seizi-table
                   beg end
                   do-force))

(defun seikana-replace (pat table beg end &optional do-force)
  "新字體・康煕字典體相互變換關數の實體"
  (save-excursion
    (save-match-data
      (goto-char beg)
      (while (re-search-forward pat end t)
        (let* ((str (match-string 0))
               (rep (gethash str table '())))
          (goto-char (match-beginning 0))
          (cond
           ((null rep)
            (error "internal error. table is inconsistent."))
           ((null (cdr rep))
            (when (or do-force
                      (y-or-n-p (format "replace with `%s'?" (car rep))))
              (replace-match (car rep))))
           (t
            (let ((r (seikana-select rep)))
              (when r
                (replace-match r)))))
          (goto-char (match-end 0))))
      (message "done."))))

(provide 'seikana-ziom)
