(ns core
  (:require
   [clojure.test :refer :all]
   [clojure.string :as s]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.pprint :refer (pprint)]))

(defn lines [path]
  (line-seq (io/reader path)))

(defn comma-seperated-longs [^String s]
  (mapv parse-long (s/split s #",")))

;; Day 9

(defn adjacent-cells [w h x y]
  (into [] (for [[i j] [[0 1] [0 -1] [1 0] [-1 0]]
                 :let [ax (+ x i)
                       by (+ y j)]
                 :when (and (nat-int? ax)
                            (nat-int? by)
                            (> w ax)
                            (> h by))]
             [ax by])))

(is (= [[0 1] [1 0]])
    (adjacent-cells 10 5 0 0))

(defn lowest-adjacent-height [{:keys [width height samples]} x y]
  (let [choices (for [[i j] (adjacent-cells width height x y)]
                  (get samples [i j] Long/MAX_VALUE))]
    (apply min choices)))

(defn lowpoints [{:keys [samples] :as hm}]
  (for [[[x y] h] samples
        :when (< h (lowest-adjacent-height hm x y))]
    {:x x
     :y y
     :h h
     :risk (inc h)}))

(defn heightmap [input]
  (let [heights (map (fn [l]
                       (map parse-long (s/split l #""))) input)
        height (count heights)
        width (count (first heights))]
    {:width width
     :height height
     :samples (into (sorted-map) (for [y (range height)
                                       x (range width)]
                                   [[x y] (nth (nth heights y) x)]))}))

(is (= 15
       (reduce + (mapv :risk (lowpoints (heightmap (lines "input/ex9")))))))

(is (= 496
       (reduce + (mapv :risk (lowpoints (heightmap (lines "input/day9")))))))

(defn basin-size [{:keys [width height samples] :as hm} p]
  (loop [[x y] [(:x p) (:y p)]
         visited #{}
         frontier []]
    (let [adk (adjacent-cells width height x y)
          adk (remove #(= 9 (samples %)) adk)
          adk (remove visited adk)
          nf (concat frontier adk)]
      (if (empty? nf)
        (conj visited [x y])
        (recur
         (first nf)
         (conj visited [x y])
         (rest nf))))))


(let [hm (heightmap (lines "input/day9"))
      lp (lowpoints hm)
      basins (map (partial basin-size hm) lp)]
  (reduce * (take-last 3 (sort (map count basins)))))
