(ns ent-lsp.grammar
  (:require [clojure.string :as str]
            [malli.core :as m]
            [sutra.parser.ent :as ent]
            [sutra.parser.resolve :as res]
            [sutra.parser.sugar :as sugar]))

(defn- err-pairs
  ([errs] (err-pairs errs []))
  ([errs path]
   (cond
     (nil? errs) []
     (string? errs) [[path errs]]
     (map? errs) (mapcat (fn [[k v]] (err-pairs v (conj path k))) errs)
     (and (sequential? errs) (every? string? errs)) (map (fn [s] [path s]) errs)
     (sequential? errs) (apply concat (map-indexed (fn [i v] (err-pairs v (conj path i))) errs))
     :else [[path "ungültiger Wert"]])))

(defn- loc-of [{:keys [by-path]} path]
  (loop [p (vec path)]
    (if-let [loc (or (:loc (by-path [:key p])) (:loc (by-path p)))]
      loc
      (if (seq p) (recur (pop p)) {:row 1 :col 1 :end-row 1 :end-col 2}))))

(defn- parse-of [kind] (get-in ent/kinds [kind :parse]))

(defn- grammar-errors [rel kind {:keys [data]}]
  (if (nil? kind)
    (ent/explain data)
    (try ((parse-of kind) data rel) nil
         (catch clojure.lang.ExceptionInfo ex (:errors (ex-data ex))))))

(defn check
  [rel kind parsed]
  (when-not (:error parsed)
    (for [[path msg] (err-pairs (try (grammar-errors rel kind parsed)
                                     (catch Throwable t
                                       {:<datei>
                                          [(str "Prüfung fehlgeschlagen: " (ex-message t))]})))]
      {:rule :grammatik :file rel :loc (loc-of parsed path)
       :vars {:message (str msg " — "
                            (str/join " > " (map #(if (keyword? %) (name %) (str %)) path)))}})))

(defn lower
  [schema]
  (try (sugar/lower-schema schema) (catch Throwable _ nil)))

(defn lean-schema
  [rel kind {:keys [data error]}]
  (when (and (not error) kind)
    (when-let [parse (parse-of kind)]
      (try (:schema (parse data rel)) (catch Throwable _ nil)))))

(defn merge-tables
  [tables]
  (apply merge-with (fn [a b] (cond (and (set? a) (set? b)) (into a b)
                                    (and (map? a) (map? b)) (merge a b)
                                    :else b))
         (remove nil? tables)))

(defn symbol-table
  [schemas]
  (merge-tables (for [s schemas :when s]
                  (try (res/symbol-table s) (catch Throwable _ nil)))))

(defn resolve-findings
  [rel kind parsed facts live]
  (when (and (not (:error parsed)) (nil? kind))
    (when-let [schema (lower (get-in parsed [:data :schema]))]
      (let [fs (try (res/findings schema live)
                    (catch Throwable t
                      [{:severity :error
                        :message (str "Auflösung fehlgeschlagen: " (ex-message t))}]))]
        (for [{:keys [severity message entity]} fs]
          {:rule :referenz :file rel
           :severity (if (= :error severity) :error :warning)
           :loc (or (some #(when (= entity (:name %)) (:name-loc %)) (:defs facts))
                    {:row 1 :col 1 :end-row 1 :end-col 2})
           :vars {:message message}})))))

(defn- mtype [s] (m/type s))
(defn- children [s] (m/children s))

(defn- alternatives
  [s]
  (case (mtype s)
    (:ref :schema :malli.core/schema) (alternatives (m/deref s))
    (:and :or) (mapcat alternatives (children s))
    :multi (mapcat #(alternatives (last %)) (children s))
    :maybe (alternatives (first (children s)))
    [s]))

(defn- step [s k]
  (mapcat (fn [a]
            (case (mtype a)
              :map    (keep (fn [[ek _ es]] (when (= ek k) es)) (children a))
              :map-of (when-not (int? k) [(second (children a))])
              (:vector :sequential :set) (when (int? k) [(first (children a))])
              :tuple  (when (int? k) (some-> (nth (children a) k nil) vector))
              nil))
          (alternatives s)))

(defn schemas-at
  [kind path]
  (reduce (fn [ss k] (mapcat #(step % k) ss)) [(ent/grammar-of kind)] path))

(defn- brief [s]
  (let [as (alternatives s)]
    (str/join " | " (distinct (for [a as]
                                (case (mtype a)
                                  :enum (str/join " " (map pr-str (children a)))
                                  (name (mtype a))))))))

(defn keys-at
  [kind path]
  (->> (for [s (schemas-at kind path) a (alternatives s) :when (= :map (mtype a))
             [k props es] (children a)]
         {:key k :required? (not (:optional props)) :type (brief es)})
       (group-by :key)
       vals
       (map first)
       (sort-by (comp name :key))))

(defn values-at
  [kind path]
  (distinct (for [s (schemas-at kind path) a (alternatives s)
                  v (case (mtype a) :enum (children a) := (children a) :boolean [true false] nil)]
              v)))
