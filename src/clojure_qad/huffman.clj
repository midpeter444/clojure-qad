(ns clojure-qad.huffman)

;; don't think I need this
(defprotocol CodeTree
  "Huffman encoded tree of letters"
  (get-chars  [this]))


(defrecord Fork [left right chars weight])
(defrecord Leaf [char weight])

(extend-type Fork
  CodeTree
  (get-chars [this] (:chars this)))

(extend-type Leaf
  CodeTree
  (get-chars [this] (vector (:char this))))


(defn union-code-tree-pair
  "returns Fork"
  [left right]
  (->Fork left right
          (into (get-chars left) (get-chars right))
          (+ (:weight left) (:weight right))))

(defn empty-or-one?
  "predicate. returns true if coll passed in has
   more than 1 elements in it"
  [coll]
  (or (nil? coll)
      (or (empty? coll)
          (empty? (rest coll)))))


(defn combine-first-two
  "combines the first two CodeTree entries into a Fork and prepends
   them to whatever else is remaining in the collection.
   Note: this method does no bounds checking, so you should
   only send it a collection with at least two CodeTree entries"
  [tree-coll]
  (vec (cons (union-code-tree-pair (first tree-coll)
                                   (second tree-coll))
             (rest (rest tree-coll)))))

(defn make-single-tree
  "steps through an ordered coll of CodeTrees and
   creates a single ordered CodeTree from it"
  [tree-coll]
  (if (empty-or-one? tree-coll)
    tree-coll
    (make-single-tree (combine-first-two tree-coll))))

(defn ordered-leaf-vec
  "Takes a map of chars to frequencies and returns an
   ordered (sorted) vector of Leaf records"
  [mfreqs]
  (->> mfreqs
       (map #(->Leaf (first %) (second %)))
       (sort #(< (:weight %) (:weight %2)))
       vec))

;; Primary external function to call
(defn create-code-tree
  "Encodes the text passed in into a single Huffman CodeTree"
  [^String text]
  (let [mfreq (frequencies text)
        vleafs (ordered-leaf-vec mfreq)
        ]
    (first (make-single-tree vleafs))
    )
  )
