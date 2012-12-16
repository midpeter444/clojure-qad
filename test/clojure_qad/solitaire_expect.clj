(ns clojure-qad.solitaire-expect
  (:use expectations
        clojure-qad.solitaire-cipher))


;;block-format
(def block1 (partition 5 "IAMHAPPIERTHANIWASBEFOREX"))
        
(expect block1 (block-format "I am happier than I was before!!"))

(expect (partition 5 "ONEORTWOSOMETIMESMOREMEXX")
        (block-format "One ... or two; sometimes more -- @me."))

(def lookup {\A 1, \B 2, \C 3, \D 4, \E 5, \F 6, \G 7, \H 8, \I 9, \J 10, \K 11, \L 12, \M 13, \N 14, \O 15, \P 16, \Q 17, \R 18, \S 19, \T 20, \U 21, \V 22, \W 23, \X 24, \Y 25, \Z 26})

;; text->num on blocks
(expect [9 1 13 8 1] (first (text->num block1)))
(expect [16 16 9 5 18] (second (text->num block1)))
(expect (count block1) (count (text->num block1)))
(expect (count (last block1)) (count (last (text->num block1))))

;; text->num on blocks
(expect (first block1) (first (num->text [[9 1 13 8 1]])))
(expect (second block1) (first (num->text [[16 16 9 5 18]])))
(expect (count block1)  (count (num->text (text->num block1))))

(def deck [1 2 3 4 5 \A])

(expect [1 2 4 3 5 \A] (move-card-down deck 3 1))
(expect [1 3 4 5 2 \A] (move-card-down deck 2 3))
(expect [2 3 4 5 \A 1] (move-card-down deck 1 5))
(expect [1 2 3 4 \A 5] (move-card-down deck 5 1))
(expect [1 \A 2 3 4 5] (move-card-down deck \A 1))
(expect [1 5 2 3 4 \A] (move-card-down deck 5 2))
