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

(defn lazy-adapton-memoize [f]
  (memoize (fn [& x] (adapt (apply f x)))))

(defn adapton-memoize [f]
  (let [f* (lazy-adapton-memoize f)]
    (fn [& x]
      (adapton-force (apply f* x)))))

(defmacro lazy-lambda-amemo
  [args & body]
  `(let [f# (lazy-adapton-memoize
             (fn ~@args ~@body))]
     (lambda args (f# ~@args))))

(defmacro lambda-amemo
  [args & body]
  `(let [f# (adapton-memoize
             (fn ~args ~@body))]
     (fn ~args (f# ~@args))))

(defmacro lazy-defn-amemo
  [f args & body]
  `(def ~f (lazy-lambda-amemo ~args ~@body)))

(defmacro defn-amemo
  [f args & body]
  `(def ~f (lambda-amemo ~args ~@body)))

(defmacro defn-avar
  [name expr]
  `(def ~name
     (micro/aref (adapt ~expr))))

(defn avar-get [v]
  (adapton-force (adapton-force v)))

(defmacro avar-set!
  [v expr]
  `(micro/set-aref! ~v (adapt ~expr)))
