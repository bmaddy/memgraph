(ns clodapton.mini-adapton
  (:require [memgraph.micro-adapton :as micro]))

(def aforce
  (let [currently-adapting (atom false)]
    (fn [a]
      (let [prev-adapting @currently-adapting
            _ (reset! currently-adapting a)
            result (micro/compute a)
            _ (reset! currently-adapting prev-adapting)]
        (when @currently-adapting
          (micro/add-dcg-edge! @currently-adapting a))
        result))))

(comment
  (def r (micro/aref 5))
  (def a (micro/make-athunk #(+ (aforce r) 3)))
  (aforce a) ;;=> 8
  (micro/set-aref! r 2)
  (aforce a) ;;=> 5

  )
