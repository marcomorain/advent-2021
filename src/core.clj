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

(defn digits [s]
  (mapv parse-long (s/split s #"")))


;; Day 11

(defn ->coords [grid]
  (into {} (for [i (range (count grid))
                 j (range (count (nth grid 0)))]
             [[i j] (nth (nth grid j) i)])))

(defn adjacents [[x y]]
  (for [i (range -1 2)
        j (range -1 2)
        :when (not (and (= 0 i)
                        (= 0 j)))]
    [(+ i x)
     (+ j y)]))

(adjacents [6 11])

(defn print-grid [grid]
  (doseq [j (range 0 10)]
    (doseq [i (range 0 10)]
      (printf "%3s" (get grid [i j] "")))
    (println))
  (println)

  grid)



(defn simulate [grid]
  (loop [grid (update-vals grid inc)
         flashed-coords #{}]
    (let [flashing-coords (->> grid
                           (filter (comp (partial <= 9) val))
                               (keys)
                               (set))
          newly-flashing (set/difference flashing-coords flashed-coords)
          overflow (mapcat adjacents newly-flashing)]
      (prn "newly flashing" newly-flashing)
      (if (empty? newly-flashing)
        (update-vals grid (fn [e]
                            (if (>= e 9)
                              0
                              e)
                            ))
        (recur (reduce (fn [g a]
                         (if (contains? g a)
                           (update g a inc)
                           g))
                       grid
                       overflow)
               (set/union flashed-coords newly-flashing))))))
      


(let [grid (->coords (map digits (lines "input/ex11")))]
  (print-grid (simulate (print-grid (simulate (print-grid grid))))))
