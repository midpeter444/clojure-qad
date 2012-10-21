(ns clojure-qad.huffman)

;; ---[ Data Structure Definition ]--- ;;
(defprotocol CodeTree
  "Huffman encoded tree of letters"
  (get-chars  [this])
  (leaf? [this])
  (decode-node [this callback tree bits chars])
  (encode-node [this callback tree bits chars])
  )

(defrecord Fork [left right chars weight])
(defrecord Leaf [char weight])

(extend-type Fork
  CodeTree
  (get-chars [this] (:chars this))

  (leaf? [this] false)

  (decode-node [this callback tree bits chars]
    (if (= 0 (first bits))
      (callback (:left this) (rest bits) chars)
      (callback (:right this) (rest bits) chars)))

  (encode-node [this callback tree bits chars]
    (let [ncontains? (fn [c node]
                       (some #(= c %) (get-chars node)))
          mnext (if (ncontains? (first chars) (:left this))
                  {:node (:left this), :dir 0}
                  {:node (:right this), :dir 1})]
      (callback (:node mnext) (conj bits (:dir mnext)) chars)))
  )
  
(extend-type Leaf
  CodeTree
  (get-chars [this] (vector (:char this)))

  (leaf? [this] true)

  (decode-node [this callback tree bits chars]
    (callback tree bits (conj chars (:char this))))

  (encode-node [this callback tree bits chars]
    (if (= (:char this) (first chars))
      (callback tree bits (rest chars))
      (throw (IllegalStateException. "Leaf Char does not match next char on list of chars"))))
  )


;; ---[ Creation Functions ]--- ;;

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
  (->> text
       frequencies
       ordered-leaf-vec
       make-single-tree
       first))


;; ---[ Decode Functions ]--- ;;

(defn decode
  "Takes a single Huffman encoded CodeTree and a sequence of 'bits'
   (0 = 'go left', 1 = 'go right') to navigate the CodeTree in order
   to decode the message. Returns the message as a string."
  [tree bit-path]
  (letfn [(fdec [subtree bits acc-chars]
            (if (empty? bits)
              (if (leaf? subtree)
                (conj acc-chars (:char subtree))
                acc-chars)
              (if (leaf? subtree)
                (fdec tree bits (conj acc-chars (:char subtree)))
                (if (= 0 (first bits))
                  (fdec (:left subtree)  (rest bits) acc-chars)
                  (fdec (:right subtree) (rest bits) acc-chars)))))]
    (apply str (fdec tree bit-path [])))
  )

(defn decode2 [tree bit-path]
  (letfn [(fdec [subtree bits acc-chars]
            (if (empty? bits)
              (conj acc-chars (:char subtree))
              (decode-node subtree fdec tree bits acc-chars)))]
    (apply str
           (lazy-seq
            (fdec tree bit-path []))))
  )


;; ---[ Encode Functions ]--- ;;

(defn encode
  "Simple (brute-force) encoding with ???"
  [tree text]
  (letfn [(fenc [subtree bits chars]
            (if (empty? chars)
              bits
              (encode-node subtree fenc tree bits chars)))]
    (fenc tree [] (seq text))))

(defn create-path-map
  "Takes a CodeTree and walks all the paths
   to generate a map of each char in the tree to its
   bit-path, which it returns."
  [tree]
  (letfn [(create-map [sub-tree bit-path]
            (if (leaf? sub-tree)
              {(:char sub-tree) bit-path}
              (merge (create-map (:left sub-tree) (conj bit-path 0))
                     (create-map (:right sub-tree) (conj bit-path 1)))))]
    (create-map tree [])))

;; is there a better way to write the fenc fn?
;; map? or recur at least? or lazy-seq?
(defn fast-encode [tree text]
  (let [path-map (create-path-map tree)
        fenc (fn fenc [chars]
               (if (empty? chars)
                 []
                 (concat (path-map (first chars)) (fenc (rest chars)))))]
    (vec (fenc (seq text)))))
