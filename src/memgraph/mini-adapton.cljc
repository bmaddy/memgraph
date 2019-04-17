(ns memgraph.mini-adapton
  (:require [memgraph.micro-adapton :as micro]))

(def adapton-force
  (let [currently-adapting (atom false)]
    (fn [a]
      (let [prev-adapting @currently-adapting
            _ (reset! currently-adapting a)
            result (micro/compute a)
            _ (reset! currently-adapting prev-adapting)]
        (when @currently-adapting
          (micro/add-dcg-edge! @currently-adapting a))
        result))))

(defmacro adapt
  [expr]
  `(micro/make-athunk
   (fn []
     ~expr)))

(defn adapton-memoize-l [f]
  (memoize (fn [& x] (adapt (apply f x)))))

(defn adapton-memoize [f]
  (let [f* (adapton-memoize-l f)]
    (fn [& x]
      (adapton-force (apply f* x)))))

(defmacro lambda-amemo-l
  [args & body]
  `(let [f# (adapton-memoize-l
             (fn ~@args ~@body))]
     (lambda args (f# ~@args))))

(defmacro lambda-amemo
  [args & body]
  `(let [f# (adapton-memoize
             (fn ~args ~@body))]
     (fn ~args (f# ~@args))))

(defmacro define-amemo-l
  [f args & body]
  `(def ~f (lambda-amemo-l ~args ~@body)))

(defmacro define-amemo
  [f args & body]
  `(def ~f (lambda-amemo ~args ~@body)))

(defmacro define-avar
  [name expr]
  `(def ~name
     (micro/aref (adapt ~expr))))

(defn avar-get [v]
  (adapton-force (adapton-force v)))

(defmacro avar-set!
  [v expr]
  `(micro/set-aref! ~v (adapt ~expr)))





(define-amemo max-tree [t]
  (cond
    (micro/adapton? t) (max-tree (adapton-force t))
    (and (coll? t) (= 1 (count t))) (max-tree (first t))
    (coll? t) (max (max-tree (first t))
                   (max-tree (rest t)))
    :else t))

(comment

  (define-amemo max-tree-path [t]
    (micro/adapton? t) (max-tree-path (adapton-force t))
    (seq? t) (if (> (max-tree (first t))
                    (max-tree (second t)))
               (conj (max-tree-path (first t)) :left)
               (conj (max-tree-path (second t)) :right))
    :else '())

  (pprint
     (macroexpand-1
      '(define-amemo max-tree-path [t]
         (micro/adapton? t) (max-tree-path (adapton-force t))
         (seq? t) (if (> (max-tree (first t))
                         (max-tree (second t)))
                    (conj (max-tree-path (first t)) :left)
                    (conj (max-tree-path (second t)) :right))
         :else '())))

  (pprint
   (macroexpand-1
    '(memgraph.mini-adapton/lambda-amemo
      [t]
      (micro/adapton? t) (max-tree-path (adapton-force t))
      (seq? t) (if (> (max-tree (first t))
                      (max-tree (second t)))
                 (conj (max-tree-path (first t)) :left)
                 (conj (max-tree-path (second t)) :right))
      :else '())))

  (define-avar lucky 7)
  (define-avar t1 [1 2])
  (define-avar t2 [3 4])
  (define-avar some-tree (list* (avar-get t1) (avar-get t2)))
  (avar-get some-tree) ;; [[1 2] 3 4]
  (max-tree some-tree) ;; 4

  )

;; TODO test these functions
;; TODO switch define-* to defn-*
;; TODO switch *-l to *-lazy
