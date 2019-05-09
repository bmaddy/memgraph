(ns memgraph.micro-adapton
  "A faithful implementation of microAdapton for reference purposes. Thread
  safety has not been tested.")

(defprotocol IAdapton
  (get-thunk   [this])
  (set-thunk!  [this v])
  (get-result  [this])
  (set-result! [this v])
  (get-sub     [this])
  (set-sub!    [this v])
  (get-super   [this])
  (set-super!  [this v])
  (get-clean?  [this])
  (set-clean?! [this v])
  (->map       [this]))

(deftype Adapton [^:volatile-mutable thunk
                  ^:volatile-mutable result
                  ^:volatile-mutable sub
                  ^:volatile-mutable super
                  ^:volatile-mutable clean?]
  IAdapton
  (get-thunk   [_] thunk)
  (set-thunk!  [_ v] (set! thunk v))
  (get-result  [_] result)
  (set-result! [_ v] (set! result v))
  (get-sub     [_] sub)
  (set-sub!    [_ v] (set! sub v))
  (get-super   [_] super)
  (set-super!  [_ v] (set! super v))
  (get-clean?  [_] clean?)
  (set-clean?! [_ v] (set! clean? v))
  (->map       [_] {:thunk thunk
                    :result result
                    :sub sub
                    :super super
                    :clean? clean?}))

(def adapton? (partial instance? Adapton))

(defn make-athunk
  [thunk]
  (->Adapton thunk 'empty #{} #{} false))

(defn declare-adapton
  []
  (make-athunk nil))

(defn add-dcg-edge!
  [super sub]
  (set-sub! super (conj (get-sub super) sub))
  (set-super! sub (conj (get-super sub) super)))

(defn del-dcg-edge!
  [super sub]
  (set-sub! super (disj (get-sub super) sub))
  (set-sub! sub (disj (get-super sub) super)))

(defn compute
  [a]
  (if (get-clean? a)
    (get-result a)
    (do
      ;; recomputing, all old subcomputations are invalid
      (doseq [x (get-sub a)]
        (del-dcg-edge! a x))
      ;; this will be clean once we recompute
      (set-clean?! a true)
      ;; evaluate the thunk and remember the result
      (set-result! a ((get-thunk a)))
      ;; recompute in case the athunk has been marked dirty in the meantime
      (recur a))))

(defn dirty!
  [a]
  (when (get-clean? a)
    (set-clean?! a false)
    (doseq [x (get-super a)]
      (dirty! x))))

(defn aref
  [v]
  (let [a (->Adapton nil v #{} #{} true)]
    (set-thunk! a #(get-result a))
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

  (pprint (->map r1))
  (pprint (->map r2))
  (pprint (->map a))
  (compute a) ;;=> -2
  (set-aref! r1 2)
  (compute a) ;;=> -8
  )
