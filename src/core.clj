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

(defn parse-digits [line]
  (let [digits (s/split line #"[\|\s]+")]
    (take-last 4 digits)))

(->> "input/ex8"
     (lines)
     (mapcat parse-digits)
     (map count)
     (filter #{2 3 4 7})
     (count))
