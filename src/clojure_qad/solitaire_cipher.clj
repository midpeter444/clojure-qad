(ns clojure-qad.solitaire-cipher
  (:require [clojure.string :as str]))

(defn block-format
  "Strip all non-alphabetical characters, uppercase all
   remaining chars and chunk into 5 char 'blocks', padding
   the last one with X's as necessary.
   Returns a lazy seq of 5 char blocks."
  [args]
  (partition 5 5 "XXXXX"
             (-> (apply str args)
                 (.replaceAll "[^a-zA-Z]" "")
                 .toUpperCase)))

(def char->num-map
  (apply hash-map (interleave (apply concat (partition 1 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
                              (iterate inc 1))))

(def num->char-map
  (apply hash-map (interleave (iterate inc 1)
                              (apply concat (partition 1 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))))

(defn text->num
  "Maps the blocked 'text' (as chunked seqs of 5 chars at a time)
   into blocked or chunked numbers with A = 1, B = 2, ... Z = 26."
  [blocks]
  (map #(map char->num-map %) blocks))

(defn encrypted? [args]
  (loop [v args]
    (when-not (empty? v)
      (println (first v))
      (println (count (first v)))
      (recur (rest v))))
  (every? #(= (count %) 5) args))

(defn encrypt [args]
  ;; (let [plain-block     (block-format args)
  ;;       keyed-block     (gen-keystream plain-block)
  ;;       plain-num-block (text->num plain-block)
  ;;       keyed-num-block (text->num keyed-block)]
  ;;   (-> (add-blocks plain-num-block keyed-num-block)
  ;;       num->text))
  "encrypt")

(defn decrypt [args]
  "decrypt")

(defn -main [& args]
  (if (encrypted? args)
    (do (println "Decrypting ...")
        (flush)
        (println (decrypt args)))
    (do (println "Encrypting ...")
        (flush)
        (println (encrypt args)))))

