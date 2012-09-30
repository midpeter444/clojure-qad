(ns clojure-qad.secret-santas
  (:require [clojure.math.combinatorics :refer [selections]]
            [clojure.string :as str]))

;; read in all names  <= only in main
;; put input names in vector <= testable
;; xxx
;; puts santa -> receiver mappings in map <= testable
;; print out map
;; (comb/combinations v 2)


;; ---[ Version 1 functions ]--- ;;

;; (first (filter (complement #{"Mrs. Gray" "Mr. Cloud"}) (shuffle v)))
;; (merge m (apply hash-map p))
(defn target
  "santa: string name
   selected: set of names already selected
   players: vector of all players
   returns map of santa -> target"
  [santa selected players]
  (let [shufdp    (shuffle players)
        dont-pick (merge selected santa)
        options   (filter (complement dont-pick) shufdp)
        choice    (first options)
        ]
    (println "---------- target -----------")
    (println "santa: " santa)
    (println "shuffled: " shufdp)
    (println "dont-pick: " dont-pick)
    (println "options: " options)
    (println "choice: " choice)
    {santa choice}
    )

  ;; {santa
  ;;  (first (filter (complement (merge selected santa)) (shuffle players)))}
  )

(defn version1 [players]
  (loop [santas players selected #{} pairs {}]
    (if (empty? santas)
      pairs
      (let [pair (target (first santas) selected players)]
        (recur (rest santas)
               (merge selected (first (vals pair)))
               (merge pairs pair))
        )
      )))

;; ---[ Version 2 functions ]--- ;;

;; this produces (from [:a :b :c :d] input):
;;   {:a [(:a :b) (:a :c) (:a :d)], :b [(:b :a) (:b :c) (:b :d)],
;;    :c [(:c :a) (:c :b) (:c :d)], :d [(:d :a) (:d :b) (:d :c)]}

(declare key->name)

;; (defn last-name2 [player]
;;   (let [f (fn [s]
;;             (println "splitting: >>" s "<<")
;;             (last (str/split s #"\s")))] 
;;     (if (keyword? player)
;;       (do
;;         (println "1last-name will return : " (f (key->name player)))
;;         (f (key->name player)))
;;       (do
;;         (println "2last-name will return : " (f player))
;;         (f player))
;;       )
;;     )
;;   )

(defn last-name [player]
  ;; (println "Sent:::" player)
  (let [f (fn [s]
            ;; (println "splitting: >>" s "<<")
            (last (str/split s #"\s")))] 
    (cond
     (nil? player) player
     (keyword? player) (f (key->name player))
     :else (f player)
     )
    )
  )

(defn possible-choices [players]
  ;; TODO: the filter needs to be adjusted once we have actual names and people
  ;; with common last names
  (group-by first (filter #(not= (first %) (second %)) (selections players 2))))

;; ;; {santa
;; ;;  (first (filter (complement (merge selected santa)) (shuffle players)))}
;; ;; TODO: may not need the santa name here as an arg ...
(defn next-pair
  "santa: String name of santa
   taken: set of names already selected (already have a santa)
   pairs: list/vector of tuples name->name
   Returns the next tuple of the santa and the chosen recipient"
  [taken pairs]
  (let [f (fn [pair]
            (cond
             (taken (second pair)) nil
             (= (last-name (first pair)) (last-name (second pair))) nil
             :else pair))]
    (first (filter f (shuffle pairs))))
  ;; (first (filter #((complement taken) (second %)) (shuffle pairs)))
  )

(defn version2 [players]
  (let [p (possible-choices players)]
    (loop [santas (keys p) selected #{} pairs []]
      ;; (println "selected: " selected)
      (if (empty? santas)
        pairs
        (let [pair (next-pair selected ((first santas) p))]
          (recur (rest santas)
                 (merge selected (second pair))
                 (conj pairs pair)
                 )
          )
        )
      )
    )
  )

;; ---[ Base methods ]--- ;;

(defn name->key
  "Takes a seq of strings and returns a seq of keywords"
  [v]
  (map keyword v))

(defn key->name
  "Takes either a single keyword or a seq of keywords.
   Returns string or seq of strings with the ':' stripped off."
  [x]
  (if (keyword? x)
    (subs (str x) 1)
    (let [kn (fn [pair]
               (map #(subs (str %) 1) pair))]
      (map kn x)
      )
    )
  )

(defn get-names []
  ;; this should read from STDIN later
  ;; [:a :b :c :d :e]
  (name->key ["Mr Gray" "Mrs Gray" "Mr Thomas" "Mrs Thomas" "Mr Matsumoto" "Mrs Matsumoto" "Mr Fulton"])
  )

(defn contains-nil? [v]
  (< 0 (count (filter nil? v))))

;; LEFT-OFF: need to filter out couples with last name so they can't select each other
;; then need to learn how to read the input from STDIN
;; and format the output to STDOUT
(defn -main
  "Run the secret-santas quiz"
  [& args]
  (loop [pairs (version2 (get-names))]
    (if-not (contains-nil? pairs)
      (key->name pairs)
      (recur (version2 (get-names)))
      )
    )
  )


