(ns memgraph.mini-adapton-test
  (:require [memgraph.mini-adapton :as sut]
            [memgraph.micro-adapton :as micro]
            [clojure.test :as t :refer [deftest is]]))

(deftest adapton-force-test
  (let [r (micro/aref 5)
        a (micro/make-athunk #(+ (sut/adapton-force r) 3))]
    (is (= 8 (sut/adapton-force a)))
    (micro/set-aref! r 2)
    (is (= 5 (sut/adapton-force a)))))

(deftest adapt-test
  (let [r1 (micro/aref 2)
        r2 (micro/aref (+ (sut/adapton-force r1) 4))
        a (sut/adapt (+ (sut/adapton-force r1)
                        (sut/adapton-force r2)))]
    (is (= 8 (sut/adapton-force a)))
    (micro/set-aref! r1 10)
    (is (= 16 (sut/adapton-force a)))))
