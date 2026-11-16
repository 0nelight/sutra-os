(ns grammar.api-facade-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rt]))

(defn api-files []
  (sort (for [d (.listFiles (io/file "src/sutra"))
              :let [f (io/file d "api.clj")]
              :when (.exists f)]
          f)))

(defn- forms [f]
  (binding [r/*read-eval* false
            r/*alias-map* (reify
                            clojure.lang.ILookup (valAt [_ k] k) (valAt [_ k _] k)
                            clojure.lang.IFn (invoke [_ k] k) (invoke [_ k _] k))]
    (let [rdr (rt/indexing-push-back-reader (slurp f))]
      (loop [acc []]
        (let [x (r/read {:eof ::eof} rdr)]
          (if (= ::eof x) acc (recur (conj acc x))))))))

(defn- arities
  [[_ _ & more]]
  (let [more (drop-while #(or (string? %) (map? %)) more)]
    (if (vector? (first more))
      [[(first more) (rest more)]]
      (map (fn [[ps & body]] [ps body]) more))))

(defn- call-of
  [body]
  (when (= 1 (count body))
    (let [[h & args :as x] (first body)]
      (when (seq? x)
        (cond
          (= 'apply h)                                   [(first args) (rest args) true]
          (and (symbol? h) (namespace h))                [h args false])))))

(defn- fixed [ps] (vec (take-while #(not= '& %) ps)))
(defn- rest-param [ps] (second (drop-while #(not= '& %) ps)))

(defn- takes? [target-arglists n variadic?]
  (some (fn [al]
          (let [f (count (fixed al)) v? (some #{'&} al)]
            (if variadic? (and v? (<= f n)) (or (= f n) (and v? (<= f n))))))
        target-arglists))

(defn problems-of
  [f]
  (let [[nsf & body] (forms f)
        api-ns (second nsf)]
    (require api-ns)
    (let [aliases (ns-aliases (the-ns api-ns))
          target  (fn [sym] (some-> (get aliases (symbol (namespace sym)))
                                    ns-name (ns-resolve (symbol (name sym)))))]
      (vec
       (for [x body
             :when (and (seq? x) (#{'def 'defn} (first x)))
             :let [nm (second x)
                   why (case (first x)
                         def  (let [v (last x)]
                                (when-not (and (symbol? v) (namespace v) (target v))
                                  "def that names no target var"))
                         defn (let [as (arities x)]
                                (or (some (fn [[ps body]]
                                            (let [[t args app?] (call-of body)
                                                  want (cond-> (fixed ps)
                                                         (rest-param ps) (conj (rest-param ps)))]
                                              (cond
                                                (nil? t)
                                                "body is not one call of a target var"
                                                (nil? (target t))
                                                (str "target " t " does not resolve")
                                                (not= (vec args) want)
                                                (str "passes " (vec args) " for params " ps)
                                                (not= (boolean app?) (boolean (rest-param ps)))
                                                "variadic params need apply, apply needs them"
                                                (not (takes? (:arglists (meta (target t)))
                                                             (count (fixed ps))
                                                             (boolean (rest-param ps))))
                                                (str "target " t " takes no arity " (count ps))
                                                (some #(re-matches #"a\d+" (str %)) ps)
                                                (str "generated param name in " ps))))
                                          as)
                                    (when (< 1 (count (distinct (map (comp first call-of second)
                                                                     as))))
                                      "arities call different targets"))))]
             :when why]
         [(str api-ns "/" nm) why])))))

(deftest every-api-var-delegates
  (doseq [f (api-files)]
    (testing (str f)
      (is (= [] (problems-of f))))))
