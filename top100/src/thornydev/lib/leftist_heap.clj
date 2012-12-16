(ns thornydev.lib.leftist-heap)

;; Implementation of a "leftist priority queue" on an immutable
;; heap data structure
;; This code was (manually) transpiled into Clojure from 
;; the Scheme version here:
;; http://programmingpraxis.com/2009/05/05/priority-queues/2/

;; ---[ core lib fns ]--- ;;

(defn pq-rank [pq] (pq 0))
(defn pq-item [pq] (pq 1))
(defn pq-lkid [pq] (pq 2))
(defn pq-rkid [pq] (pq 3))

(defn error [fnsym msg]
  (throw (IllegalArgumentException. (str msg " in fn " fnsym))))

(def pq-empty (vector 0 'pq-empty 'pq-empty 'pq-empty))
(defn pq-empty? [pq] (= pq pq-empty))

;; TODO: can we use recur or lazy-seq instead?
(defn pq-merge [lt? pq1 pq2]
  (letfn [(pq-swap [item lkid rkid]
            (if (< (pq-rank rkid) (pq-rank lkid))
              (vector (+ (pq-rank rkid) 1) item lkid rkid)
              (vector (+ (pq-rank lkid) 1) item rkid lkid)))]

    (cond (pq-empty? pq1) pq2
          (pq-empty? pq2) pq1

          (lt? (pq-item pq2) (pq-item pq1))
          (pq-swap (pq-item pq2) (pq-lkid pq2)
                   (pq-merge lt? pq1 (pq-rkid pq2)))

          :else (pq-swap (pq-item pq1) (pq-lkid pq1)
                         (pq-merge lt? (pq-rkid pq1) pq2)))))

(defn pq-insert [lt? pq x]
  (pq-merge lt? (vector 1 x pq-empty pq-empty) pq))

;; works like peek
(defn pq-first [pq]
  (if (pq-empty? pq)
    (error 'pq-first "empty priority queue")
    (pq-item pq)))

;; works like pop
(defn pq-rest [lt? pq]
  (if (pq-empty? pq)
    (error 'pq-rest "empty priority queue")
    (pq-merge lt? (pq-lkid pq) (pq-rkid pq))))


;; ---[ conversion fns ]--- ;;

(defn list->pq [lt? xs]
  (loop [xs xs pq pq-empty]
    (if (not (seq xs))
      pq
      (recur (rest xs) (pq-insert lt? (first xs) pq)))))

(defn pq->list [lt? pq]
  (loop [pq pq xs []]
    (if (pq-empty? pq)
      xs
      (recur (pq-rest lt? pq) (conj xs (pq-first pq))))))

(defn pq-sort [lt? xs]
  (pq->list lt? (list->pq lt? xs)))


(comment
  ;; Example usage
  (pq-sort > '(3 4 5 17 13 22 -6 1)))
;; => [22 17 13 5 4 3 1 -6]
