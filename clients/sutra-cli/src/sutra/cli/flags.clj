(ns sutra.cli.flags
  (:require [clojure.string :as str]
            [sutra.cli.table :as table]))

(defn comma-list
  [s]
  (vec (remove str/blank? (str/split (str s) #","))))

(defn- by-word
  [spec]
  (into {} (for [[n f] spec, w (cons (str "--" n) (:aliases f))] [w [n f]])))

(defn- flag-word? [w] (str/starts-with? (str w) "--"))

(defn- takes-word?
  [f next-word]
  (cond (:bool? f)                  false
        (contains? f :optional-val) (and (some? next-word) (not (flag-word? next-word)))
        :else                       (some? next-word)))

(defn- with-value
  [opts {:keys [key coerce also to vector? repeat?]} v]
  (let [v (if coerce (coerce v) v)]
    (cond to       (to opts v)
          vector?  (update opts key (fnil conj []) v)
          repeat?  (update opts key #(if % (str % "," v) v))
          :else    (merge (assoc opts key v) also))))

(defn parse
  [spec init ws]
  (let [index (by-word spec)]
    (loop [[w & more] ws opts init pos [] unknown [] missing []]
      (let [[_ f]     (get index w)
            next-word (first more)]
        (cond
          (nil? w) {:opts opts :words pos :unknown unknown :missing missing}

          (and f (or (nil? (:only-if f)) ((:only-if f) pos)))
          (let [takes? (takes-word? f next-word)]
            (cond
              takes?
              (recur (rest more) (with-value opts f next-word) pos unknown missing)
              (or (:bool? f) (contains? f :optional-val))
              (recur more (with-value opts f (if (:bool? f) true (:optional-val f)))
                     pos unknown missing)
              :else
              (recur more opts pos unknown (conj missing w))))

          (flag-word? w) (recur more opts (conj pos w) (conj unknown w) missing)

          :else (recur more opts (conj pos w) unknown missing))))))

(defn- parse-secret-arg
  [s]
  (let [[lhs val] (str/split (str s) #"=" 2)
        [src fld] (when lhs (str/split lhs #"\." 2))]
    (when (and src fld val)
      [[(keyword src) (keyword fld)] val])))

(defn- checkout?
  [words]
  (:checkout? (table/process-words (if (= "code" (first words)) (second words) (first words)))))

(def ^:private leading? empty?)

(def globals
  {"yes"        {:key :yes? :bool? true :line? true}
   "strict"     {:key :strict? :bool? true :line? true}
   "json"       {:key :json? :bool? true :line? true}
   "text"       {:key :text? :bool? true :line? true}
   "no-color"   {:key :no-color? :bool? true :line? true}
   "lang"       {:key :lang :line? true}
   "fields"     {:key :fields :coerce comma-list :also {:json? true} :line? true}
   "branch"     {:key :branch :only-if leading?}
   "project"    {:key :project :only-if leading?}
   "instance"   {:key :instance :only-if leading?}
   "ontology"   {:key :ontology :only-if leading?}
   "zip"        {:key :zip? :bool? true :only-if checkout?}
   "archive"    {:key :archive? :bool? true :only-if checkout?}
   "with-files" {:key :with-files? :bool? true :only-if checkout?}
   "out"        {:key :out :only-if checkout?}
   "only"       {:key :only :only-if checkout?
                 :coerce #(set (map (fn [s] (keyword (str/replace s "_" "-")))
                                    (str/split (str %) #",")))}
   "repository" {:key :repository :only-if checkout? :coerce keyword}
   "creds"      {:key :creds-path}
   "base"       {:key :base-url}
   "secret"     {:to (fn [opts v]
                       (if-let [[path x] (parse-secret-arg v)]
                         (assoc-in opts (cons :secrets path) x)
                         opts))}})

(def line-globals
  (into {} (filter (comp :line? val)) globals))

(defn refuse-missing!
  [{:keys [missing]}]
  (when-let [f (first missing)]
    (throw (ex-info (str f " needs a value") {:flag (str/replace f #"^-+" "")}))))
