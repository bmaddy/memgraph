(ns memgraph.mini-adapton-test
  (:require [memgraph.mini-adapton :as sut]
            [memgraph.micro-adapton :as micro]
            [clojure.test :as t :refer [deftest is testing]]))

(deftest adapton-force-test
  (testing "from miniAdapton paper section 4.1"
    (let [r (micro/aref 5)
          a (micro/make-athunk #(+ (sut/adapton-force r) 3))]
      (is (= 8 (sut/adapton-force a)))
      (micro/set-aref! r 2)
      (is (= 5 (sut/adapton-force a))))))

(deftest adapt-test
  (testing "from miniAdapton paper section 4.4"
    (let [r1 (micro/aref 2)
          r2 (micro/aref (+ (sut/adapton-force r1) 4))
          a (sut/adapt (+ (sut/adapton-force r1)
                          (sut/adapton-force r2)))]
      (is (= 8 (sut/adapton-force a)))
      (micro/set-aref! r1 10)
      (is (= 16 (sut/adapton-force a))))))

(deftest defn-avar-test
  (testing "from miniAdapton paper section 4.4"
    (map #(ns-unmap *ns* %) '[v1 v2 b])
    (sut/defn-avar v1 2)
    (sut/defn-avar v2 (+ (sut/avar-get v1) 4))
    (sut/defn-avar b (+ (sut/avar-get v1) (sut/avar-get v2)))
    (is (= 8 (sut/avar-get b)))
    (sut/avar-set! v1 10)
    (is (= 24 (sut/avar-get b)))))

(deftest avar-test
  (testing "anonymous avar"
    (let [v1 (sut/avar 2)
          v2 (sut/avar (+ (sut/avar-get v1) 4))
          b (sut/avar (+ (sut/avar-get v1) (sut/avar-get v2)))]
      (is (= 8 (sut/avar-get b)))
      (sut/avar-set! v1 10)
      (is (= 24 (sut/avar-get b))))))

(deftest extended-example-test
  (testing "from miniAdapton paper section 4.5"
    (testing "max-tree and max-tree-path"
      (map #(ns-unmap *ns* %) '[lucky t1 t2 some-tree])
      (sut/defn-avar lucky 7)
      (sut/defn-avar t1 [1 2])
      (sut/defn-avar t2 [3 4])
      (sut/defn-avar some-tree [(sut/avar-get t1) (sut/avar-get t2)])

      (sut/defn-amemo max-tree
        ;; Finds the max- imum number in a tree made of pairs and numbers
        [t]
        (cond
          (micro/adapton? t) (max-tree (sut/adapton-force t))
          (coll? t) (max (max-tree (first t))
                         (max-tree (second t)))
          :else t))

      (sut/defn-amemo max-tree-path
        ;; Finds the path from the root of the tree to the maximum number
        [t]
        (cond
          (micro/adapton? t) (max-tree-path (sut/adapton-force t))
          (coll? t) (if (> (max-tree (first t))
                           (max-tree (second t)))
                      (cons 'left (max-tree-path (first t)))
                      (cons 'right (max-tree-path (second t))))
          :else []))

      (is (= [[1 2] [3 4]] (sut/avar-get some-tree)))
      (is (= 4 (max-tree some-tree)))
      (is (= '[right right] (max-tree-path some-tree)))

      (sut/avar-set! t2 5)
      (is (= [[1 2] 5] (sut/avar-get some-tree)))
      (is (= 5 (max-tree some-tree)))
      (is (= '[right] (max-tree-path some-tree)))
      (is (= 5 (max-tree (second (sut/avar-get some-tree)))))
      (is (= [] (max-tree-path (second (sut/avar-get some-tree)))))

      (sut/avar-set! t2 (vector 20 (* 3 (sut/avar-get lucky))))
      (is (= [[1 2] [20 21]] (sut/avar-get some-tree)))
      (is (= 21 (max-tree some-tree)))
      (is (= '[right right] (max-tree-path some-tree)))

      (sut/avar-set! lucky 3)
      (is (= [[1 2] [20 9]] (sut/avar-get some-tree)))
      (is (= 20 (max-tree some-tree)))
      (is (= '[right left] (max-tree-path some-tree))))))
