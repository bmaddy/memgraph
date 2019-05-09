(ns memgraph.html-test
  (:require [clojure.test :as t]
            [memgraph.micro-adapton :refer [adapton?]]
            [memgraph.mini-adapton :refer [avar avar-get avar-set! adapton-force defn-amemo]]))

(defn q
  [items]
  (into (clojure.lang.PersistentQueue/EMPTY) items))

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
  {"4" (q ["5" "9"]),
   "0" (q ["8"]),
   "1" (q ["2" "9"]),
   "2" (q ["4" "8"]),
   "7" (q ["7"]),
   "9" (q ["7" "1" "2" "8"]),
   "8" (q ["0" "7"]),
   "5" (q ["0"])})

(defn make-data-avar
 "Adapton version of data"
  []
  {"4" (avar (q ["5" "9"])),
   "0" (avar (q ["8"])),
   "1" (avar (q ["2" "9"])),
   "2" (avar (q ["4" "8"])),
   "7" (avar (q ["7"])),
   "9" (avar (q ["7" "1" "2" "8"])),
   "8" (avar (q ["0" "7"])),
   "5" (avar (q ["0"]))})

(def data-avar (make-data-avar))

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
    :else (vec
           (list* :ul
                  (mapv (fn [k]
                          (if-let [descendants (hiccup data (get data k) (dec max-depth))]
                            [:li k descendants]
                            [:li k])) ks)))))

(declare hiccup-memo)

(defn hiccup-memo*
  #_(hiccup-memo data ["1"] 3)
  [data ks max-depth]
  (cond
    (zero? max-depth) nil
    :else (vec
           (list* :ul
                  (mapv (fn [k]
                          (if-let [descendants (hiccup-memo data (get data k) (dec max-depth))]
                            [:li k descendants]
                            [:li k])) ks)))))

(def hiccup-memo (memoize hiccup-memo*))

(defn-amemo hiccup-amemo
  #_(hiccup-amemo data-avar ["1"] 3)
  [data ks max-depth]
  (cond
    (adapton? ks) (hiccup-amemo data (adapton-force ks) max-depth)
    (zero? max-depth) nil
    :else (vec
           (list* :ul
                  (mapv (fn [k]
                          (if-let [descendants (hiccup-amemo data (get data k) (dec max-depth))]
                            [:li k descendants]
                            [:li k])) ks)))))

(comment
  (= (hiccup       data      ["1"] 20)
     (hiccup-memo  data      ["1"] 20)
     (hiccup-amemo data-avar ["1"] 20))

  ;; both memoized versions are faster
  (time (count (hiccup data ["1"] 34)))
  (time (count (hiccup-memo data ["1"] 34)))
  (time (count (hiccup-amemo data-avar ["1"] 34)))


  ;; NOTE: these speed tests are pretty sloppy and inconclusive because this
  ;; implementation uses Clojure's persistent data structures for the adapton
  ;; implementation. Really they should be mutable variables. I did it this way
  ;; initially because it was easy and was hoping to see a massive improvement
  ;; with the adapton algorithm.
  ;;
  ;; * This in no way suggests that adapton is slower.

  ;; What if we do it more times?

  ;; memoized persistent data structures
  (time (dotimes [n 10000000]
          (hiccup-memo data ["1"] 100)))
  ;; memoized adapton
  (time (dotimes [n 10000000]
          (hiccup-amemo data-avar ["1"] 100)))


  ;; updating a value to something different each iteration
  (def updates
    (vec
     (take 10000
           (map vector
                (repeatedly #(rand-nth (keys data)))
                (repeatedly #(rand-nth (keys data)))))))

  (time
   (count
    (reduce (fn [d [k v]]
              (hiccup-memo d ["1"] 10)
              ;; for debugging updates
              #_(println (->> d
                              (sort-by first)
                              (map second)
                              (map vec)))
              (update d k #(-> % pop (conj v))))
            data
            updates)))

  (def data-avar (make-data-avar))

  (time
   (count
    (reduce (fn [d [k v]]
              (hiccup-amemo d ["1"] 10)
              ;; for debugging updates
              #_(println (->> d
                              (sort-by first)
                              (map second)
                              (map avar-get)
                              pprint))
              (let [av (get d k)
                    updated (-> av avar-get pop (conj v))]
                (avar-set! av updated))
              d)
            data-avar
            updates)))


  ;; These aren't really fair tests
  ;; * should review the implementation more closely
  ;; * should be using criterium

  ;; Possible future tests:
  ;; * use mutable queue instead of persistent one
  ;; * emulate dragging a bunch of dependent points using dependent avars
  )
