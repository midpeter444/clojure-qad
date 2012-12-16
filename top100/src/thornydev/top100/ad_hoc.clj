(ns thornydev.top100.ad-hoc)

(defn distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn add-point [m dist v]
  (let [m2 (if (m dist)
             (update-in m [dist] conj v)
             (assoc m dist [v]))]
    (let [m3 (if (> dist (:hd m2))
               (merge m2 {:hd dist})
               m2)]
      (update-in m3 [:ct] inc))))

(defn next-highest-dist [m]
  {:hd (first (filter #(m %) (range (dec (:hd m)) -1 -1)))})

(defn remove-key [m dist]
  (if (= dist (:hd m))
    (merge (dissoc m dist) (next-highest-dist m))
    (dissoc m dist)))

(defn remove-point [m dist]
  (let [m2 (if (<= (count (m dist)) 1)
             (remove-key m dist)
             (update-in m [dist] subvec 1))]
    (update-in m2 [:ct] dec)))

(defn mk-sift-fn [max-size]
  (fn [m pt]
    (let [dist (distance pt)]
      (if (< (:ct m) max-size)
        (add-point m dist pt)
        (if (< dist (:hd m))
          (-> m (remove-point (:hd m)) (add-point dist pt))
          m)))))

(def init-map {:hd Long/MIN_VALUE, :ct 0})

(defn topN [points max-size]
  (reduce (mk-sift-fn max-size) init-map points))
