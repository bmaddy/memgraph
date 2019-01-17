(ns memgraph.micro-adapton
  "Not thread safe. A faithful implementation of microAdapton for reference purposes.")

(defn adapton
  [thunk result sub super clean?]
  (atom {:thunk thunk
         :result result
         :sub sub
         :super super
         :clean? clean?}))

(defn set-result! [a r] (swap! a assoc :result r))

(defn set-sub! [a s] (swap! a assoc :sub s))

(defn set-super! [a s] (swap! a assoc :super s))

(defn set-clean?! [a c] (swap! a assoc :clean? c))

(defn make-athunk
  [thunk]
  (adapton thunk 'empty #{} #{} false))

(defn add-dcg-edge!
  [super sub]
  (set-sub! super (conj (:sub @super) sub))
  (set-super! sub (conj (:super @sub) super)))

(defn del-dcg-edge!
  [super sub]
  (set-sub! super (disj (:sub @super) sub))
  (set-sub! sub (disj (:super @sub) super)))

(defn compute
  [a]
  (if (:clean? @a)
    (:result @a)
    (do
      (doseq [x (:sub @a)]
        (del-dcg-edge! a x))
      (set-clean?! a true)
      (set-result! a ((:thunk @a)))
      (compute a))))

(defn dirty!
  [a]
  (when (:clean? @a)
    (set-clean?! a false)
    (doseq [x (:super @a)]
      (println :dirtying (select-keys @a [:result :clean?]))
      (dirty! x))))

(defn aref
  [v]
  (let [a (adapton nil v #{} #{} true)]
    (swap! a assoc :thunk #(:result @a))
    a))

(defn set-aref!
  [a v]
  (set-result! a v)
  (dirty! a))

(comment

  (def r1 (aref 8))
  (def r2 (aref 10))
  (def a (make-athunk (fn []
                        (add-dcg-edge! a r1)
                        (add-dcg-edge! a r2)
                        (- (compute r1)
                           (compute r2)))))

  (merge (select-keys @r1 [:result :clean?]) {:sub-count (-> @r1 :sub count) :super-count (-> @r1 :supers count)})
  (merge (select-keys @r2 [:result :clean?]) {:sub-count (-> @r2 :sub count) :super-count (-> @r2 :supers count)})
  (merge (select-keys @a [:result :clean?]) {:sub-count (-> @a :sub count) :super-count (-> @a :supers count)})
  (compute a) ;;=> -2
  (set-aref! r1 2)
  (compute a) ;;=> -8
  )
