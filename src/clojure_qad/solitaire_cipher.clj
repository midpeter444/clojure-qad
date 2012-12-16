(ns clojure-qad.solitaire-cipher
  (:require [clojure.string :as str]))

;; Encryption Notes
;; Enc #1 => block-format
;; Enc #2 => not yet impl
;; Enc #3 => text->num


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

(defn num->text
  "Maps the blocked 'numbers' (as chunked seqs of 5 numbers at a time)
   into blocked or chunked numbers with A = 1, B = 2, ... Z = 26."
  [nblocks]
  (map #(map num->char-map %) nblocks))


(defn encrypted?
  "Determines whether the argument list (from the command line)
   is encrypted, by detecting if the input is in blocks of 5 chars."
  [args]
  (every? #(= (count %) 5) args))

(defn add-blocks
  "Takes two numbered 'blocks' (vectors/seqs) of numbers and
   returns a vec with the sum at each position"
  [nblock1 nblock2]
  (mapv + nblock1 nblock2))

;; ---[ Keystream functions ]--- ;;
(def cdeck (vec (concat (take 52 (iterate inc 1)) [\A \B])))

(defn move-card-down
  "Move card (value in deck) n places 'down' the deck"
  [deck card n]
  (let [pos (.indexOf deck card)]
    (if (> (count deck) (+ pos n))
      (vec (concat (remove #{card} (subvec deck 0 (+ pos n 1)))
                   [card]
                   (subvec deck (+ pos n 1))))
      (let [npos (inc (mod (+ pos n) (count deck)))]
        (vec (concat (subvec deck 0 npos)
                     [card]
                     (remove #{card} (subvec deck npos))))
        
        )
      )
    )
  )

(defn gen-keystream
  ""
  [char-block]
  )
;; ---[ END Keystream functions ]--- ;;

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

