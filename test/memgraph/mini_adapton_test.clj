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

(deftest define-avar-test
  (testing "from miniAdapton paper section 4.4"
    (map #(ns-unmap *ns* %) '[v1 v2 b])
    (sut/define-avar v1 2)
    (sut/define-avar v2 (+ (sut/avar-get v1) 4))
    (sut/define-avar b (+ (sut/avar-get v1) (sut/avar-get v2)))
    (is (= 8 (sut/avar-get b)))
    (sut/avar-set! v1 10)
    (is (= 24 (sut/avar-get b)))))

(deftest extended-example-test
  (testing "from miniAdapton paper section 4.5"
    (testing "max-tree"
      (map #(ns-unmap *ns* %) '[lucky t1 t2 some-tree])
      (sut/define-avar lucky 7)
      (sut/define-avar t1 [1 2])
      (sut/define-avar t2 [3 4])
      (sut/define-avar some-tree (list* (sut/avar-get t1) (sut/avar-get t2)))
      (sut/define-amemo max-tree
        [t]
        (cond
          (micro/adapton? t) (max-tree (sut/adapton-force t))
          (and (coll? t) (= 1 (count t))) (max-tree (first t))
          (coll? t) (max (max-tree (first t))
                         (max-tree (rest t)))
          :else t))
      (is (= [[1 2] 3 4] (sut/avar-get some-tree)))
      (is (= 4 (max-tree some-tree))))

    (testing "max-tree-path"
      (map #(ns-unmap *ns* %) '[lucky t1 t2 some-tree])
      (sut/define-avar lucky 7)
      (sut/define-avar t1 [1 2])
      (sut/define-avar t2 [3 4])
      (sut/define-avar some-tree (list* (sut/avar-get t1) (sut/avar-get t2)))
      (sut/define-amemo max-tree-path
        [t]
        (cond
          (micro/adapton? t) (max-tree-path (sut/adapton-force t))
          ;; working here
          (and (coll? t) (= 1 (count t))) (max-tree (first t))
          (coll? t) (max (max-tree (first t))
                         (max-tree (rest t)))
          :else t))
      (is (= [[1 2] 3 4] (sut/avar-get some-tree)))
      (is (= 4 (max-tree some-tree))))))
