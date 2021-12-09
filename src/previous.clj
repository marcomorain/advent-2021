(ns previous
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


;; Day 1

(->> "input/day1"
     (lines)
     (map parse-long)
     (partition 2 1)
     (filter #(apply < %))
     (count))


(->> "input/day1"
     (lines)
     (map parse-long)
     (partition 3 1)
     (map #(apply + %))
     (partition 2 1)
     (filter #(apply < %))
     (count))

;; Day 2

(def directions
  {"forward" [1 0]
   "down" [0 1]
   "up" [0 -1]})

(defn- parse-command [s]
  (let [[d x] (s/split s #" ")]
    (repeat (parse-long x)
            (get directions d))))

(let [pairs (->> "input/day2"
                 (lines)
                 (mapcat parse-command))]
  (* (reduce + (map first pairs))
     (reduce + (map second pairs))))


(defn parse-command-2 [s]
  (let [[d x] (s/split s #" ")]
    [d (parse-long x)]))

(defn advance-submarine [[pos depth aim :as state] [dir mag :as command]]
  (case dir
    "down" [pos depth (+ aim mag)]
    "up"  [pos depth (- aim mag)]
    "forward" [(+ pos mag) (+ depth (* aim mag)) aim]))

(let [[pos depth] (reduce advance-submarine
                          [0 0 0]
                          (->> "input/day2"
                               (io/reader)
                               (line-seq)
                               (map parse-command-2)))]
  (* pos depth))


;; Day 3

(defn bin->long [binary]
  (Long/parseLong binary  2))

(defn transpose [xs]
  (apply map vector xs))

(defn sort-digits
  "Returns [least-common most-common]"
  [digits]
  (->> digits
       (frequencies)
       (sort-by val)
       (map key)))

(->> "input/day3"
     (lines)
     (map vec)
     (transpose)
     (map sort-digits)
     (transpose)
     (map s/join)
     (map bin->long)
     (apply *))


(defn digit-filter [gas digits]
  (let [ones (get (frequencies digits) \1 0)
        zeros (get (frequencies digits) \0 0)]
    (case gas
      :oxygen
      (if (= ones zeros)
        (partial = \1)
        (partial = (if (> ones zeros) \1 \0)))
      :co2
      (if (= ones zeros)
        (partial = \0)
        (partial = (if (> ones zeros) \0 \1))))))

(defn filter-rating [gas rows]
  (loop [n 0
         rows rows]
    (if (= 1 (count rows))
      (Long/parseLong (s/join (first rows)) 2)
      (let [digits (map #(nth % n) rows)
            selector (digit-filter gas digits)]
        (recur (inc n)
               (filter (fn [ds]
                         (selector (nth ds n)))
                       rows))))))

(defn filter-oxygen [digits]
  (filter-rating :oxygen digits))

(defn filter-C02 [digits]
  (filter-rating :co2 digits))

(->> "input/day3"
     (lines)
     (map vec)
     ((juxt filter-oxygen filter-C02))
     (apply *))


;; Day 4

(defn ->board [lines]
  (let [rows (into [] lines)
        cols (into [] (transpose lines))]
    {:rows rows
     :cols cols}))

(defn load-bingo [path]
  (let [[draw & input] (lines path)
        draws (map parse-long (s/split draw #","))
        parse (fn [line]
                (map parse-long
                     (filter (complement s/blank?)
                             (s/split line #"\s+"))))
        items (->> (map parse input)
                   (filter not-empty)
                   (partition 5))]
    {:draws draws
     :boards (map ->board items)}))

(defn play-bingo [draws board]
  (loop [n 1]
    (let [call (take n draws)
          check (fn [{:keys [rows cols]}]
                  (first (filter
                          (fn [line]
                            (set/superset? (set call) (set line)))
                          (concat rows cols))))]
      (when (> n (count draws))
        (throw (ex-info "no winner" {:call call})))
      (if (check board)
        {:board board
         :call call}
        (recur (inc n))))))

(defn score-bingo [{:keys [board call]}]
  (let [numbers (apply concat (:rows board))
        sum (reduce + (remove (set call) numbers))]
    (* sum (last call))))


;; ex4 -> 4512
;; day4 -> 82440
(let [{:keys [draws boards]} (load-bingo "input/day4")
      wins (sort-by (comp count :call) (map (partial play-bingo draws) boards))]
  {:best (-> wins first score-bingo)
   :worst (-> wins last score-bingo)})




(defn print-diagram [diagram]
  (doseq [y (range 10)
          x (range 10)]
    (printf "%s" (get diagram [x y] "."))
    (when (= x 9)
      (println)))
  diagram)

;; Day 5

(defn between [a b]
  (cond
    (= a b) (repeat a)
    (< a b)
    (range a (inc b))
    :else
    (reverse (range b (inc a)))))

(is (= [1 1 1] (take 3 (between 1 1))))
(is (= [3 4 5] (between 3 5)))
(is (= [7 6 5 4 3 2] (between 7 2)))

(defn zip [& colls]
  (apply map vector colls))

(is (= [[1 :a "a"] [2 :b "b"] [3 :c "c"]]
       (zip [1 2 3] [:a :b :c] ["a" "b" "c"])))

(defn parse-vent [s]
  (let [[x1 y1 x2 y2] (->> s
                           (re-matches #"(\d+),(\d+) -> (\d+),(\d+)")
                           (rest)
                           (map parse-long))]
    (zip (between x1 x2)
         (between y1 y2))))

(are [vent points] (= points (parse-vent vent))
  "1,1 -> 1,3" [[1,1] [1,2] [1,3]]
  "9,7 -> 7,7" [[9,7] [8,7] [7,7]]
  "1,1 -> 3,3" [[1,1] [2,2] [3,3]]
  "9,7 -> 7,9" [[9,7] [8,8] [7,9]])

(defn vent->diagram [diagram vent]
  (update diagram vent (fnil inc 0)))

(->> "input/day5"
     (lines)
     (mapcat parse-vent)
     (reduce vent->diagram {})
     (print-diagram)
     (filter (comp (partial < 1) val))
     (count))


;; Day 7

(defn median [coll]
  (-> coll count even? assert)
  (nth (sort coll)
       (/ (count coll) 2)))


(defn mean [coll]
  (long (Math/round (double (/ (reduce + coll)
                        (count coll))))))



(defn abs [n]
  (Math/abs (int n)))

(defn crab-fuel [crabs cost-fn dest]
  (reduce + (map (partial cost-fn dest) crabs)))

(defn cost-linear [dest crab]
  (abs (- dest crab)))

(defn cost-accelerate [dest crab]
  (reduce + (range (inc (cost-linear dest crab)))))

(defn search [path dest-fn cost-fn]
  (let [crabs (-> path
                  (lines)
                  (first)
                  (comma-seperated-longs))
        dest (dest-fn crabs)]
    {:fuel (crab-fuel crabs cost-fn dest)
     :dest dest}))

(is (= {:fuel 37, :dest 2}
       (search "input/ex7" median cost-linear)))

(is (= {:fuel 168, :dest 5}
       (search "input/ex7" mean cost-accelerate)))

(is (= {:fuel 329389, :dest 330}
       (search "input/day7" median cost-linear)))

(is (=  {:fuel 86397080, :dest 459}
         (search "input/day7" mean cost-accelerate)))



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
