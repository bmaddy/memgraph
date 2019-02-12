(ns memgraph.micro-adapton
  "Not thread safe. A faithful implementation of microAdapton for reference purposes.")

;; a future method that is similar to make-hierarchy
;; (def graph (atom {:subs {} :supers {}}))
#_{:subs {a #{r1 r2}}
   :supers {r1 #{a}
            r2 #{a}}
   :athunks {a (fn ...)}
   :results {a -2}
   :clean? {a true}}

;; maybe use this with a lock for performance?
;; maybe build up a list of changes that need to be made and do them all at once?
#_{[:subs a] #{r1 r2}
   [:clean? a] true
   ... ...}

;; (deftype Adapton [thunk result sub super clean?])
;; (defrecord Adapton [state]
;;   clojure.lang.Atom)
#_(gen-class
 :name memgraph.micro-adapton.Adapton
 :extends clojure.lang.Atom)

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
  (set-clean?! [this v]))

;; Yes, this :volatile-mutable is bad practice. Just following the paper
;; directly here. It seems to be focused on single threaded environments.
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
  (set-clean?! [_ v] (set! clean? v)))

;; I am working on implementing miniAdapton in Clojure. These Adaptons are
;; wrappers that can contain any value. I need to write some code that looks
;; like this:
;; ```
;; (if (adapton? x)
;;   (fetch-the-value-of x)
;;   x)
;; ```
;; these adaptons need to be mutable. Atoms will work for now, but I need to be
;; able to differentiate between adaptons and atoms so I know if I should fetch
;; the value or not. This suggests to me that I need a new type. Ideally, I
;; wouldn't wrap my atom, but just have my new type work the exact same way. So
;; here's my question: How do I create a new type that is a subclass of Atom and
;; works the exact same way?
;; another option would be to use metadata with an ::adapton? key on an atom


#_(defn adapton
  [thunk result sub super clean?]
  (atom {:thunk thunk
         :result result
         :sub sub
         :super super
         :clean? clean?}))

;; (defn set-result! [a r] (swap! a assoc :result r))

;; (defn set-sub! [a s] (swap! a assoc :sub s))

;; (defn set-super! [a s] (swap! a assoc :super s))

;; (defn set-clean?! [a c] (swap! a assoc :clean? c))

(defn make-athunk
  [thunk]
  (->Adapton thunk 'empty #{} #{} false))

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
      (doseq [x (get-sub a)]
        (del-dcg-edge! a x))
      (set-clean?! a true)
      (set-result! a ((get-thunk a)))
      (compute a))))

(defn dirty!
  [a]
  (when (get-clean? a)
    (set-clean?! a false)
    (doseq [x (get-super a)]
      (println :dirtying {:result (get-result a)
                          :clean? (get-clean? a)})
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


(defn as-map
  [a]
  {:thunk (get-thunk a)
   :result (get-result a)
   :sub (get-sub a)
   :super (get-super a)
   :clean? (get-clean? a)})

(comment

  (def r1 (aref 8))
  (def r2 (aref 10))
  (def a (make-athunk (fn []
                        (add-dcg-edge! a r1)
                        (add-dcg-edge! a r2)
                        (- (compute r1)
                           (compute r2)))))

  (merge (select-keys (as-map r1) [:result :clean?]) {:sub-count (-> r1 as-map :sub count) :super-count (-> r1 as-map :supers count)})
  (merge (select-keys (as-map r2) [:result :clean?]) {:sub-count (-> r2 as-map :sub count) :super-count (-> r2 as-map :supers count)})
  (merge (select-keys (as-map a) [:result :clean?]) {:sub-count (-> a as-map :sub count) :super-count (-> a as-map :supers count)})
  (compute a) ;;=> -2
  (set-aref! r1 2)
  (compute a) ;;=> -8
  )
