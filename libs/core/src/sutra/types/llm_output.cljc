(ns sutra.types.llm-output
  #?(:clj (:require [clojure.string :as str])))

(def scalars #{:string :boolean :number :integer})

(defn problem
  [spec]
  (cond
    (scalars spec) nil
    (and (vector? spec) (= :enum (first spec)))
    (when-not (and (seq (rest spec)) (every? #(or (string? %) (keyword? %)) (rest spec)))
      ":enum needs at least one value, each a string or keyword")
    (and (vector? spec) (= :list (first spec)) (= 2 (count spec)))
    (problem (second spec))
    (and (map? spec) (map? (:struct spec)) (seq (:struct spec)) (= 1 (count spec)))
    (some (fn [[k t]] (if (keyword? k)
                        (some->> (problem t) (str (name k) ": "))
                        (str "struct field " (pr-str k) " is not a keyword")))
          (:struct spec))
    :else (str "unknown output type " (pr-str spec)
               " — :string :boolean :number :integer [:enum …] [:list t] {:struct {f t}}")))

(defn json-schema
  [spec]
  (cond
    (= :string spec)  {"type" "string"}
    (= :boolean spec) {"type" "boolean"}
    (= :number spec)  {"type" "number"}
    (= :integer spec) {"type" "integer"}
    (= :enum (first spec)) {"type" "string" "enum" (mapv name (rest spec))}
    (= :list (first spec)) {"type" "array" "items" (json-schema (second spec))}
    :else {"type" "object"
           "properties" (into {} (for [[k t] (:struct spec)] [(name k) (json-schema t)]))
           "required" (mapv name (keys (:struct spec)))
           "additionalProperties" false}))

(defn- parse-num [s]
  (when (and (string? s) (re-matches #"\s*-?\d+(\.\d+)?([eE][-+]?\d+)?\s*" s))
    #?(:clj (Double/parseDouble (str/trim s)) :cljs (js/parseFloat s))))

(defn- fail [path msg] (throw (ex-info msg {::path path})))

(defn- coerce* [spec v path]
  (cond
    (= :string spec)
    (if (string? v) v (fail path (str "expected a string, got " (pr-str v))))

    (= :boolean spec)
    (cond (boolean? v) v
          (#{"true" "false"} v) (= "true" v)
          :else (fail path (str "expected a boolean, got " (pr-str v))))

    (= :number spec)
    (cond (number? v) v
          (parse-num v) (parse-num v)
          :else (fail path (str "expected a number, got " (pr-str v))))

    (= :integer spec)
    (let [n (if (number? v) v (parse-num v))]
      (if (and n (== n (Math/floor n)))
        (long n)
        (fail path (str "expected an integer, got " (pr-str v)))))

    (= :enum (first spec))
    (or (some #(when (= (name %) (if (keyword? v) (name v) v)) %) (rest spec))
        (fail path (str "expected one of " (pr-str (mapv name (rest spec))) ", got " (pr-str v))))

    (= :list (first spec))
    (if (sequential? v)
      (vec (map-indexed (fn [i x] (coerce* (second spec) x (conj path i))) v))
      (fail path (str "expected a list, got " (pr-str v))))

    :else
    (if (map? v)
      (let [by-name (into {} (map (fn [[k x]] [(name k) x])) v)]
        (into {} (for [[k t] (:struct spec)]
                   (if (contains? by-name (name k))
                     [k (coerce* t (get by-name (name k)) (conj path k))]
                     (fail (conj path k) (str "missing field " (name k)))))))
      (fail path (str "expected an object, got " (pr-str v))))))

(defn coerce
  [spec v]
  (try {:value (coerce* spec v [])}
       (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
         (if-let [p (::path (ex-data e))]
           {:error (ex-message e) :path p}
           (throw e)))))
