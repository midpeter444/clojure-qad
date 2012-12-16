(ns thornydev.top100.heap
  (:require [thornydev.lib.leftist-heap
             :refer [pq-first pq-rest pq-insert pq-empty]]))

(defn distance [[x y]]
  (+ (java.lang.Math/abs x) (java.lang.Math/abs y)))

(defn dist-lt? [pt1 pt2]
  (< (distance pt2) (distance pt1)))

(defn heap-sift [pq pt]
  (if (dist-lt? pt (pq-first pq))
    pq
    (pq-insert dist-lt? (pq-rest dist-lt? pq) pt)))

(defn top-heap [points max-size]
  (let [[add-all xs] (split-at max-size points)
        init-q (reduce #(pq-insert dist-lt? % %2) pq-empty add-all)]
    (reduce heap-sift init-q xs)))



(comment
  (def points1 (reverse (for [x (range 2 5) y (range 10 13)] [x y])))
  (count points1)
  (top-heap points1 2)
  (def spl (split-at 4 points1))
  (count spl)
  (spl 0)
  (spl 1)
  )
