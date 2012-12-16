(ns thornydev.top100.test
  (:use clojure.test)
  (:require [thornydev.top100.heap :refer [top-heap dist-lt? distance]]
            [thornydev.top100.sorted-set :refer [topNSS]]
            [thornydev.top100.ad-hoc :refer [topN]]
            [thornydev.lib.leftist-heap :refer [pq->list]]))


(def points1 (reverse (for [x (range 2 5) y (range 10 13)] [x y])))
(def points2 (shuffle (for [x (range 10 22) y (range 6)] [x y])))

(deftest all-algorithems-return-same-results
  (testing "points1 dataset: keep closest 4"
    (let [exp1 #{[2 10] [3 10] [2 11] [4 10]}
          exp2 #{[2 10] [3 10] [2 11] [3 11]}
          exp3 #{[2 10] [3 10] [2 11] [2 12]}
          heap-act (set (pq->list dist-lt? (top-heap points1 4)))
          sorted-set-act (topNSS points1 4)
          ad-hoc-act (set (apply concat (filter coll? (vals (topN points1 4)))))
          ]
      (is (or (= exp1 heap-act) (= exp2 heap-act) (= exp3 heap-act)))
      (is (or (= exp1 sorted-set-act) (= exp2 sorted-set-act) (= exp3 sorted-set-act)))
      (is (or (= exp1 ad-hoc-act) (= exp2 ad-hoc-act) (= exp3 ad-hoc-act)))
      ))
  (testing "points2 dataset: keep top 12"
    (let [heap-act (set (pq->list dist-lt? (top-heap points2 12)))
          sorted-set-act (topNSS points2 12)
          ad-hoc-act (set (apply concat (filter coll? (vals (topN points2 12)))))
          heap-distances (sort (map distance heap-act))
          sset-distances (sort (map distance sorted-set-act))
          adhoc-distances (sort (map distance ad-hoc-act))]
      (is (= heap-distances sset-distances))
      (is (= heap-distances adhoc-distances))
      ))
  )

(println (run-tests 'thornydev.top100.test))
