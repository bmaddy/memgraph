(ns memgraph.html-test
  (:require [clojure.test :as t]
            [memgraph.micro-adapton :refer [adapton?]]
            [memgraph.mini-adapton :refer [avar avar-get adapton-force defn-amemo]]))

(def data
  "A random graph of data"
  #_(let [edges 15
        nodes 10
        elements (map str (take nodes (range)))]
    (->> #(vector (rand-nth elements) (rand-nth elements))
         repeatedly
         (take edges)
         (group-by first)
         (map (fn [[k vs]]
                [k (set (map second vs))]))
         (into {})))
  {"4" #{"5" "9"},
   "0" #{"8"},
   "1" #{"2" "9"},
   "2" #{"4" "8"},
   "7" #{"7"},
   "9" #{"7" "1" "2" "8"},
   "8" #{"0" "7"},
   "5" #{"0"}})

(def data-avar
 "Adapton version of data"
 {"4" (avar #{"5" "9"}),
  "0" (avar #{"8"}),
  "1" (avar #{"2" "9"}),
  "2" (avar #{"4" "8"}),
  "7" (avar #{"7"}),
  "9" (avar #{"7" "1" "2" "8"}),
  "8" (avar #{"0" "7"}),
  "5" (avar #{"0"})})

;; https://en.wikipedia.org/wiki/Incremental_computing#/media/File:Incremental_computing.svg
;; input-1 -> P -> output-1
;; Δ-input -> ΔP -> Δ-output
;; input-2 -> P -> output-2


;; try generating hiccup style html

(defn hiccup
  #_(hiccup data ["1"] 3)
  [data ks max-depth]
  (cond
    (zero? max-depth) nil
    :else (list* :ul
                 (mapv (fn [k]
                         (if-let [descendants (hiccup data (get data k) (dec max-depth))]
                           [:li k descendants]
                           [:li k])) ks))))

(declare hiccup-memo)

(defn hiccup-memo*
  #_(hiccup-memo data ["1"] 3)
  [data ks max-depth]
  (cond
    (zero? max-depth) nil
    :else (list* :ul
                 (mapv (fn [k]
                         (if-let [descendants (hiccup-memo data (get data k) (dec max-depth))]
                           [:li k descendants]
                           [:li k])) ks))))

(def hiccup-memo (memoize hiccup-memo*))

(defn-amemo hiccup-amemo
  #_(hiccup-amemo data ["1"] 3)
  [data ks max-depth]
  (cond
    (adapton? ks) (hiccup-amemo data (adapton-force ks) max-depth)
    (zero? max-depth) nil
    :else (list* :ul
                 (mapv (fn [k]
                         (if-let [descendants (hiccup-amemo data (get data k) (dec max-depth))]
                           [:li k descendants]
                           [:li k])) ks))))

(comment
  (= (hiccup       data ["1"] 20)
     (hiccup-memo  data ["1"] 20)
     (hiccup-amemo data ["1"] 20))

  (time (count (hiccup data ["1"] 34)))
  (time (count (hiccup-memo data ["1"] 34)))
  (time (count (hiccup-amemo data ["1"] 34)))

  ;; memoized persistent data structures
  (time (dotimes [n 10000000]
          (hiccup-memo data ["1"] 500)))
  ;; memoized adapton
  (time (dotimes [n 10000000]
          (hiccup-amemo data ["1"] 500)))

  ;; updating a value to something different each iteration
  ;; emulating dragging and dependent variables

  )


;; try generating dom commands

#_(defn P
  "Returns a DOM element that displays data."
  [data]
  (fn [node]
    (doseq [[label value] data
            :let [label-node (js/document.createTextNode )]]
      (.appendChild node (js/document.createElement "li")))))

;; I'm not sure we'd have to write this one...
#_(defn delta-P
  "Given a sequence of changes, returns a function that changes the DOM?")
