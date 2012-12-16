(ns clojure-qad.secret-santas
  (:require [clojure.math.combinatorics :refer [selections]]
            [clojure.string :as str]))


(declare key->name)

(defn last-name [player]
  (let [f (fn [s]
            (last (str/split s #"\s")))] 
    (cond
     (nil? player) player
     (keyword? player) (f (key->name player))
     :else (f player))))

(defn possible-choices
  "Takes a seq of players (names as keywords).
   Returns a map with a key for each player mapped to all
   possible pairs of santa mappings."
  [players]
  (group-by first (filter #(not= (first %) (second %)) (selections players 2))))

;; ;; {santa
;; ;;  (first (filter (complement (merge selected santa)) (shuffle players)))}
(defn next-pair
  "taken: set of names already selected (already have a santa)
   pairs: list/vector of tuples name->name
   Returns a randomly chosen tuple of the santa and the chosen recipient
   that includes a target that is not already taken and does not have
   the same last name of the santa"
  [taken pairs]
  (let [f (fn [pair]
            (cond
             (taken (second pair)) nil
             (= (last-name (first pair)) (last-name (second pair))) nil
             :else true))]
    (first (filter f (shuffle pairs))))
  )

(defn- make-selections-may-return-nil [players]
  (let [p (possible-choices players)]
    (loop [santas (keys p) selected #{} pairs []]
      ;; (println "selected: " selected)
      (if (empty? santas)
        pairs
        (let [pair (next-pair selected ((first santas) p))]
          (recur (rest santas)
                 (merge selected (second pair))
                 (conj pairs pair)))))))

(declare contains-nil?)

(defn make-selections [players]
  (loop [pairs (make-selections-may-return-nil players)]
    (if-not (contains-nil? pairs)
      (key->name pairs)
      (recur (make-selections-may-return-nil players))
      )
    )
  )
;; ---[ Base methods ]--- ;;

(defn name->key
  "Takes a seq of strings and returns a seq of keywords"
  [v]
  (map keyword v))

(defn key->name
  "Takes either a single keyword or a seq of keyword tuples,
   such as [[:a :b] '(:c :d)].
   Returns string or seq of strings with the ':' stripped off."
  [x]
  (if (keyword? x)
    (subs (str x) 1)
    (let [kn (fn [pair]
               (map #(subs (str %) 1) pair))]
      (map kn x))))

;; method for testing only -> move to test/expect codebase
(defn- get-names []
  (name->key ["Mr Gray" "Mrs Gray" "Mr Thomas" "Mrs Thomas" "Mr Matsumoto" "Mrs Matsumoto" "Mr Fulton"]))

(defn contains-nil?
  "Returns true if the vector/list/seq passed in contains nil
   as a value"
  [v]
  (< 0 (count (filter nil? v))))

(defn read-input
  "Takes in the input from the user (STDIN) and returns the names as a
   seq of keywords (names mapped to their corresponding keywords."
  []
  (println "Enter players names, one per line.  Type :done when finished.")
  (loop [input (read-line) players []]
    (if (= ":done" input)
      (name->key players)
      (recur (read-line) (conj players (str/trim input))))))

(defn format-output [v]
  (println "--- Secret Santa Pairs ---")
  (dorun (map #(println (first %) "->" (second %)) v)))

(defn -main
  "Run the secret-santas quiz"
  [& args]
  (-> (read-input)
      make-selections
      format-output))


