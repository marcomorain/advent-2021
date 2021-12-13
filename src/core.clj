(ns core
  (:require
   [clojure.test :refer :all]
   [clojure.string :as s]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.data :refer (diff)]
   [clojure.pprint :refer (pprint)]))

(defn lines [path]
  (line-seq (io/reader path)))

(defn comma-seperated-longs [^String s]
  (mapv parse-long (s/split s #",")))

(defn digits [s]
  (mapv parse-long (s/split s #"")))

(defn caves [s]
  (let [[a b] (s/split s #"-")]
    [a b]))

(defn connections [acc [a b]]
  (-> acc
      (update a conj b)
      (update b conj a)))


(defn small? [cave]
  (= cave (s/lower-case cave)))

(is (small? "a"))
(is (small? "ab"))
(is (not (small? "D")))

(defn visited? [visited cave]
  (and (small? cave)
       (contains? visited cave)))

(is (not (visited? #{} "a")))
(is (visited? #{"a"} "a"))
(is (not (visited? #{"B"} "B")))


(defn search* [caves start end visited path]
  (let [at (last path)]
    (if (= at end)
      [path]
      (let [next-caves (remove (partial visited? visited)
                               (get caves at))
            results (doall (for [next-cave next-caves]
                             (search* caves start end
                                      (conj visited at)
                                      (conj path next-cave))))]
        (apply concat (remove empty? results))))))


;     start
;     /   \
; c--A-----b--d
;     \   /
;      end

(defn search [caves start end]
  (sort (search* caves start end #{} [start])))

(defn part-1 [path]
  (let [cave (reduce connections {} (map caves (lines path)))]
    (count (search cave "start" "end"))))

(is (= 10 (part-1 "input/ex12")))
(is (= 19 (part-1 "input/small12")))
(is (= 226 (part-1 "input/large12")))
(is (= 4338 (part-1 "input/day12")))

(defn can-visit? [visited cave]
  (cond
    (= "start" cave) false
    (not (small? cave)) true
    :else
    (let [visits (get visited cave 0)]
      (if (zero? visits)
        true
        (= 1 (->> visited vals sort last))))))

(are [visited cave] (can-visit? visited cave)
  {} "a"
  {} "end"
  {"a" 1} "a"
  {"a" 1 "b" 1} "a"
  {"a" 1 "b" 2} "c"
  {"A" 3} "A"
  {"start" 1 "b" 1} "b")

(are [visited cave] (not (can-visit? visited cave))
  {} "start"
  {"a" 1 "b" 2} "a"
  {"a" 1 "b" 2} "b"
  {"a" 1 "b" 1 "c" 2} "c"
  {"start" 1, "dc" 2, "kj" 1} "kj")

(defn record-visit [visited cave]
  (if (small? cave)
    (update visited cave (fnil inc 0))
    visited))

(are [before cave after] (= after (record-visit before cave))
  {} "a" {"a" 1}
  {"a" 1} "a" {"a" 2}
  {"b" 1} "a" {"a" 1 "b" 1}
  {} "B" {})


(defn search2* [caves start end visited path]
  (let [at (last path)]
    (if (= at end)
      [path]
      (let [next-visited (record-visit visited at)
            next-caves (filter (partial can-visit? next-visited)
                               (get caves at))
            results (doall (for [next-cave next-caves]
                             (search2* caves start end
                                       next-visited
                                       (conj path next-cave))))]
        (apply concat (remove empty? results))))))


;     start
;     /   \
; c--A-----b--d
;     \   /
;      end

; start,A,b,A,c,A,b,A,end

(defn search2 [caves start end]
  (search2* caves start end {} [start]))

(defn part-2 [path]
  (let [cave (reduce connections {} (map caves (lines path)))]
    (search2 cave "start" "end")))



(are [expected path] (= expected (count (part-2 path)))
  36  "input/ex12"
  103  "input/small12"
  3509  "input/large12"
  4338  "input/day12")
