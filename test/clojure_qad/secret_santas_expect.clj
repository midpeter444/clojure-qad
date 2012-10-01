(ns clojure-qad.secret-santas-expect
  (:use expectations
        clojure-qad.secret-santas))

;; ---[ Test Base Functions ]--- ;;

;; name->key
(expect [:a :b (keyword "Mr Gray")]
        (name->key ["a" "b" "Mr Gray"]))

;; key->name : seq of keywords
(expect "a" (key->name :a))
(expect "Mr Gray" (key->name (keyword "Mr Gray")))
(expect [["a" "b"]] (key->name [[:a :b]]))
(expect ['("a" "b") '("Mr Gray" "Mrs Green")]
        (key->name [[:a :b] [(keyword "Mr Gray") (keyword "Mrs Green")]]))

;; contains-nil?
(expect true (contains-nil? [nil]))
(expect false (contains-nil? [false]))
(expect false (contains-nil? []))
(expect false (contains-nil? [1 2 :a]))
(expect true (contains-nil? '(1 2 :a :b nil :c)))

;; ---[ Functions that implement logic specific to the program logic & data structures ]--- ;;

(def mr-gray (keyword "Mr Gray"))
(def mrs-gray (keyword "Mrs Gray"))
(def mr-green (keyword "Mr Green"))
(def mrs-green (keyword "Mrs Green"))
(def mr-blue (keyword "Mr Blue"))
(def vplayers [mr-gray mrs-gray mr-green mrs-green mr-blue])
(def pc (possible-choices vplayers))

;; last-name
(expect nil (last-name nil))
(expect "Gray" (last-name mr-gray))
(expect "Gray" (last-name "Mr. Gray"))

;; possible-choices
(expect {:a [[:a :b] [:a :c]]
         :b [[:b :a] [:b :c]]
         :c [[:c :a] [:c :b]]}
        (possible-choices [:a :b :c]))

(expect 6 (count (possible-choices [:a :b :c :d :e :f])))

(expect 5 (count pc))
(expect 4 (count (mrs-green pc)))
;; mrs-green should not map onto herself
(expect 0 (count (filter #(#{mrs-green} (second %)) (mrs-green pc))))
  
;; next-pair
(dotimes [_ 10]
  (let [tuple (next-pair #{mrs-green} (mr-green pc))]
    (expect true ((complement #{mrs-green mr-green}) (second tuple)))))

;; make-selections
(dotimes [_ 25]
  (expect (count vplayers) (count (make-selections vplayers)))
  (expect (reduce #(conj % (key->name %2)) #{} vplayers)
          (reduce #(conj % (first %2)) #{} (make-selections vplayers)))
  (expect (reduce #(conj % (key->name %2)) #{} vplayers)
          (reduce #(conj % (second %2)) #{} (make-selections vplayers)))
  ;; ensure that last names of santa->recipient pairs never match
  (expect []
          (filter #(= (last-name (first %)) (last-name (second %))) (make-selections vplayers)))
  )
