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

(comment
  (def r (micro/aref 5))
  (def a (micro/make-athunk #(+ (adapton-force r) 3)))
  (adapton-force a) ;;=> 8
  (micro/set-aref! r 2)
  (adapton-force a) ;;=> 5

  )

(defmacro adapt
  [expr]
  `(micro/make-athunk
   (fn []
     ~expr)))

(defn adapton-memoize-l [f]
  (memoize (fn [x] (adapt (apply f x)))))

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

(comment
  (def r1 (micro/aref 2))
  (def r2 (micro/aref (+ (adapton-force r1) 4)))
  (def a (adapt (+ (adapton-force r1)
                   (adapton-force r2))))

  (adapton-force a) ;;=> 8
  (micro/set-aref! r1 10)
  (adapton-force a) ;;=> 16

  )

(defmacro define-avar
  [name expr]
  `(def ~name
     (micro/aref (adapt ~expr))))

(defn avar-get [v]
  (adapton-force (adapton-force v)))

(defmacro avar-set!
  [v expr]
  `(micro/set-aref! ~v (adapt ~expr)))

(comment
  (define-avar v1 2)
  (define-avar v2 (+ (avar-get v1) 4))
  (define-avar b (+ (avar-get v1) (avar-get v2)))
  (avar-get b) ;;=> 8
  (avar-set! v1 10)
  (avar-get b) ;;=> 24

  )

(comment
  (define-amemo max-tree [t]
    (cond
      (micro/adapton? t) (max-tree (adapton-force t))
      (seq? t) (max (max-tree (first t))
                    (max-tree (second t)))
      :else t))

  (pprint
   (macroexpand-1
    '(define-amemo max-tree [t]
       (cond
         (micro/adapton? t) (max-tree (adapton-force t))
         (seq? t) (max (max-tree (first t))
                       (max-tree (second t)))
         :else t))))

  (pprint
   (macroexpand-1
    '(memgraph.mini-adapton/lambda-amemo
      [t]
      (cond
        (micro/adapton? t) (max-tree (adapton-force t))
        (seq? t) (max (max-tree (first t)) (max-tree (second t)))
        :else t))))

  (let [f__195745__auto__ (adapton-memoize
                           (fn [t]
                             (cond
                               (micro/adapton? t) (max-tree (adapton-force t))
                               (seq? t) (max (max-tree (first t)) (max-tree (second t)))
                               :else t)))]
    (fn [t] (f__195745__auto__ t)))


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

  )

;; TODO test these functions
;; TODO switch define-* to defn-*
;; TODO switch *-l to *-lazy
