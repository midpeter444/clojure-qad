(ns clojure-qad.huffman-expect
  (:use expectations
        clojure-qad.huffman))

;; ---[ basic Protocol setup ]--- ;;

(def fork1 (->Fork nil nil [\a \c] 22))
(def leafz (->Leaf \z 3))
(def leafx (->Leaf \x 2))
(def leafb (->Leaf \b 2))
(def leafa (->Leaf \a 1))



;; ---[ union-code-tree-pair ]--- ;;

(expect 22 (:weight fork1))
(expect [\a \c] (:chars fork1))
(expect [\a \c] (get-chars fork1))

(expect 3 (:weight leafz))
(expect \z (:char leafz))
(expect [\z] (get-chars leafz))

;; ---[ union-code-tree-pair ]--- ;;
(def fork2 (union-code-tree-pair leafx leafz))

(expect 5 (:weight fork2))
(expect [\x \z] (get-chars fork2))
(expect leafx (:left fork2))
(expect leafz (:right fork2))

;; ---[ empty-or-one? ]--- ;;
(expect true (empty-or-one? nil))
(expect true (empty-or-one? []))
(expect true (empty-or-one? [leafz]))
(expect false (empty-or-one? [fork1 leafz]))

;; ---[ combine-first-two ]--- ;;
(def tc-coll2 [leafa leafb])
(expect [(->Fork leafa leafb [\a \b]
                 (+ (:weight leafa) (:weight leafb)))]
        (combine-first-two tc-coll2))

(def tc-coll3 [leafa leafb leafx])
(expect [(->Fork leafa leafb [\a \b]
                 (+ (:weight leafa) (:weight leafb)))
         leafx]
        (combine-first-two tc-coll3))

(def tc-coll4 [leafa leafb leafx leafz])
(def fork-ab (->Fork leafa leafb [\a \b]
                     (+ (:weight leafa) (:weight leafb))))
(expect [fork-ab leafx leafz]
        (combine-first-two tc-coll4))


;; ---[ ordered-leaf-vec ]--- ;;
(def ord-vec-ayx [(->Leaf \a 1) (->Leaf \y 1) (->Leaf \s 3)])
(expect ord-vec-ayx (ordered-leaf-vec (frequencies "sassy")))

;; ---[ make-single-tree ]--- ;;
(def fork-abx (first (combine-first-two [fork-ab leafx])))
(def fork-abxz (first (combine-first-two [fork-abx leafz])))

(expect nil (make-single-tree nil))
(expect [] (make-single-tree []))
(expect [leafx] (make-single-tree [leafx]))
(expect [fork-ab] (make-single-tree [leafa leafb]))
(expect [fork-abx] (make-single-tree [leafa leafb leafx]))
(expect [fork-abxz] (make-single-tree [leafa leafb leafx leafz]))

;; ---[ create-code-tree ]--- ;;
(def text-abxz "abbxxzzz")
(def ord-vec-abxz [(->Leaf \a 1) (->Leaf \b 2) (->Leaf \x 2) (->Leaf \z 3)])
(expect ord-vec-abxz (ordered-leaf-vec (frequencies text-abxz)))
(expect [fork-abxz] (make-single-tree ord-vec-abxz))
;; some weirdness in the expectations library won't let me
;; compare the two directory - so embed the results in a single-entry vec
(expect [fork-abxz] (vector (create-code-tree text-abxz)))

;; ---[ decode ]--- ;;
(def tree-sassy (create-code-tree "sassy"))
(expect "ass" (decode tree-sassy [0 0 1 1]))
(expect "say" (decode tree-sassy [1 0 0 0 1]))

(expect "ass" (decode2 tree-sassy [0 0 1 1]))
(expect "say" (decode2 tree-sassy [1 0 0 0 1]))


;; ---[ encode ]--- ;;
(expect [0 0 1 1] (encode tree-sassy "ass"))
(expect [1 0 0 0 1] (encode tree-sassy "say"))


;; ---[ fast-encode ]--- ;;

(def pm (create-path-map tree-sassy))
(expect {\s [1], \y [0 1], \a [0 0]} pm)
(expect [0 0 1 1] (fast-encode tree-sassy "ass"))
(expect [1 0 0 0 1] (fast-encode tree-sassy "say"))
