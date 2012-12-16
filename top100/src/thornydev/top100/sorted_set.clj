(ns thornydev.top100.sorted-set)

(defn distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn dist-then-first [pt1 pt2]
  (let [dist1 (distance pt1)
        dist2 (distance pt2)]
    (if (= dist1 dist2)
      (> (first pt1) (first pt2))
      (> dist1 dist2))))

(defn mk-sorted-sift-fn [max-size]
  (fn [sst pt]
    (if (< (count sst) max-size)
      (conj sst pt)
      (let [dist-high (distance (first sst))
            dist-curr (distance pt)]
        (if (< dist-curr dist-high)
          (-> sst (disj (first sst)) (conj pt))
          sst)))))

(defn topNSS [points max-size]
  (reduce (mk-sorted-sift-fn max-size)
          (sorted-set-by dist-then-first)
          points))
