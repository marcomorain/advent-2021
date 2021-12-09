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


;; Day 8

;; Part 1 
(defn parse-digits [line]
  (let [digits (s/split line #"[\|\s]+")]
    (take-last 4 digits)))

(->> "input/day8"
     (lines)
     (mapcat parse-digits)
     (map count)
     (filter #{2 3 4 7})
     (count))


;; Part 2

(defn parse-attempts [line]
  (let [digits (s/split line #"[\|\s]+")]
    (drop-last 4 digits)))

(defn digit-with-length [n digits]
  (->> digits
       (map set)
       (filter (comp (partial = n) count))
       (set)))

(def counts {0 6
             1 2
             2 5
             3 5
             4 4
             5 5
             6 6
             7 3
             8 7
             9 6})

(defn deduce-digits [line]
  (let [digits (parse-attempts line)
        options (into {} (for [i (range 10)]
                           [i (digit-with-length (counts i) digits)]))
        contains (fn [haystack needle]
                   (filter #(set/superset? % needle) haystack))
        one (first (options 1))
        three (first (contains (options 3) one))
        four (first (options 4))
        seven (first (options 7))
        eight (first (options 8))
        be (set/difference eight three)
        zero (first (contains (contains (options 6) one) be))
        nine (first (remove #{zero} (contains (options 9) one)))
        six (first (remove #{nine zero} (options 6)))
        two-five (group-by #(empty? (set/difference % nine))
                           (remove #{three} (options 2)))
        two (first (two-five false))
        five (first (two-five true))]
    {zero 0
     one 1
     two 2
     three 3
     four 4
     five 5
     six 6
     seven 7
     eight 8
     nine 9}))

(defn compute [line]
  (let [digits (deduce-digits line)
        display (map set (parse-digits line))
        nums (for [d display]
               (get digits d))]
    (parse-long (apply str nums))))

(->> "input/day8"
     (lines)
     (map compute)
     (reduce +))



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
