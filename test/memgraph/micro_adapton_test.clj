(ns memgraph.micro-adapton-test
  (:require [memgraph.micro-adapton :as sut]
            [clojure.test :as t :refer [deftest is testing]]))

(deftest micro-adapton-test
  (testing "from miniAdapton paper Section 3"
    (let [r1 (sut/aref 8)
          r2 (sut/aref 10)
          a (sut/declare-adapton)]
      (sut/set-thunk! a (fn []
                          (sut/add-dcg-edge! a r1)
                          (sut/add-dcg-edge! a r2)
                          (- (sut/compute r1)
                             (sut/compute r2))))
      (is (= -2 (sut/compute a)))
      (sut/set-aref! r1 2)
      (is (= -8 (sut/compute a))))))

(deftest adapton?-test
  (is (= true (sut/adapton? (sut/aref 5))))
  (is (= false (sut/adapton? 5))))
