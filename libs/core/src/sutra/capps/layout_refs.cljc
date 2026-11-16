(ns sutra.capps.layout-refs
  (:require [clojure.string :as str]
            [sutra.capps.bind :as bind]
            [sutra.capps.catalog :as cat]
            [sutra.capps.walk :as walk]))

(defn- rid-like? [x] (and (string? x) (str/starts-with? x "ri.")))

(defn- param-keys
  [node]
  (concat (when (map? (:params node)) (keep cat/name-kw (keys (:params node))))
          (for [[k v] node
                :when (and (keyword? k) (str/ends-with? (name k) "-param") (some? v))]
            (cat/name-kw v))))

(defn- action-problems
  [node path actions]
  (let [a  (:action node)
        nm (when-not (rid-like? a) (cat/name-kw a))]
    (when (and nm actions)
      (if-let [known (actions nm)]
        (when (seq known)
          (for [p (distinct (param-keys node))
                :when (and p (not (contains? known p)))]
            {:rule :action-param :action nm :param p :path path}))
        [{:rule :action-unknown :action nm :path path}]))))

(def ^:private row-preserving
  #{:source :from :linked :of :where :filter :sort :order-by :limit :search :distinct? :when})

(defn- declaration
  [src vars]
  (bind/value src {:vars vars :read (fn [_ _ _ entry _] {:value entry})} nil))

(defn- source-type
  [src vars types]
  (loop [src src seen #{}]
    (let [src (if (= :val (bind/form-id src)) (declaration src vars) src)
          v   (when (map? src) (cat/name-kw (:from src)))]
      (when (and (map? src) (every? row-preserving (keys src)))
        (if (and v (contains? vars v) (not (seen v)))
          (recur {:val v} (conj seen v))
          (let [t (cat/name-kw (or (:source src) (:from src) (:linked src)))]
            (when (some? (types t)) t)))))))

(defn- item-ref [node] (when (= 1 (count node)) (or (:item node) (:item-not node))))

(defn- item-problem
  [node path item-type types]
  (let [k (cat/name-kw (item-ref node))
        fields (types item-type)]
    (when (and k fields (not (contains? fields k)))
      [{:rule :item-field :field k :type item-type :path path}])))

(defn- row-type
  [types {:keys [item-type scope] :as ctx} node k]
  (assoc ctx :item-type
         (cond
           (= :item (walk/fills-below node k)) (source-type (:items node) scope types)
           (cat/row-unknown? node k)           nil
           :else                               item-type)))

(defn- with-module-derived
  [types layout]
  (let [derived (:derived-properties layout)
        more    (when (map? derived)
                  (reduce-kv (fn [m t props]
                               (cond-> m
                                 (map? props)
                                 (update (cat/name-kw t) (fnil into #{})
                                         (keep cat/name-kw) (keys props))))
                             {} derived))]
    (if (seq more)
      (fn [t] (let [fs (types t)] (if-let [ps (and fs (more t))] (into fs ps) fs)))
      types)))

(defn problems
  [layout {:keys [item-type types actions]}]
  (let [types (with-module-derived (or types {}) layout)]
    (walk/visit
     (fn [x {:keys [path] t :item-type}]
       (concat
        (when (and (not (walk/leaf-ref? x)) (some? (:action x)))
          (action-problems x path actions))
        (when (item-ref x) (item-problem x path t types))))
     layout
     {:references? true
      :down        (partial row-type types)
      :ctx         {:item-type item-type}})))

(defn message
  [{:keys [rule action param field type]}]
  (case rule
    :action-unknown (str "Aktion " (pr-str action) " gibt es nicht")
    :action-param   (str "Aktion " (pr-str action) " hat keinen Parameter " (pr-str param))
    :item-field     (str "Objekttyp " (pr-str type) " hat kein Feld " (pr-str field))))
