(ns sutra.parser.fn-params
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def limit 20)

(defn- read-all
  [code]
  (let [rdr (java.io.PushbackReader. (java.io.StringReader. (str code)))]
    (binding [*read-eval* false]
      (try
        (loop [forms []]
          (let [f (read {:eof ::eof :read-cond :allow} rdr)]
            (if (= ::eof f) forms (recur (conj forms f)))))
        (catch RuntimeException _ nil)))))

(defn- arglists
  [[head & more]]
  (let [body (cond->> more (#{'defn 'defn- 'defmacro} head) rest)
        body (drop-while #(not (or (vector? %) (seq? %))) body)]
    (if (vector? (first body))
      [(first body)]
      (keep #(when (and (seq? %) (vector? (first %))) (first %)) body))))

(defn- fn-form? [x]
  (and (seq? x) (contains? '#{fn fn* defn defn- defmacro} (first x))))

(defn over-limit
  [code]
  (when-let [forms (read-all code)]
    (vec (for [form  forms
               x     (tree-seq coll? seq form)
               :when (fn-form? x)
               args  (arglists x)
               :let  [k (count (take-while #(not= '& %) args))]
               :when (> k limit)]
           {:name (when (symbol? (second x)) (name (second x))) :params k}))))

(defn llm-models
  [code]
  (->> (tree-seq coll? seq (vec (read-all code)))
       (keep #(when (and (seq? %) (symbol? (first %)) (= "use-llm" (name (first %)))
                         (map? (second %)))
                (some-> (:model (second %)) str)))
       distinct
       vec))

(defn with-llm-model
  [code model]
  (str/replace code #"(:model\s+)(\"[^\"]*\"|:[^\s,}\)]+)"
               (fn [[_ key _]] (str key (pr-str (str model))))))
