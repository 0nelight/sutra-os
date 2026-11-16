(ns sutra.parser.lint.support
  (:require [sutra.kit.coerce :refer [->kw]]
            [clojure.string :as str]
            [sutra.parser.impact :as impact]
            [sutra.parser.spec :as spec]
            [sutra.types.forms :as forms]
            [sutra.types.rid :as rid]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(def app-description-limit
  250)

(def top-level-page-limit
  5)

(def ->name rid/->name)

(defn log-generator
  [schema nm]
  (let [k (->kw (->name nm))]
    (some (fn [[an a]]
            (when (map? a)
              (let [an (->kw (->name an))]
                (when (= k (forms/log-type-of an a)) an))))
          (:actions schema))))

(defn section-entries
  [section name-field]
  (for [[k spec] section
        :let [nm (or (some-> (get spec name-field) ->name)
                     (when-not (rid/rid? k) (->name k)))]
        :when nm]
    [nm spec]))

(defn ot-name [schema rid]
  (or (some-> (get-in schema [:object-types rid]) :object-type-name ->name)
      (->name rid)))

(def cryptic-re
  #"(?i)^(dataset|table|type|object|obj|col|column|temp|tmp|d|t|x)\d*$|^\d+$")

(def code-attr-re
  #"(?i)(^|[_-])(plz|zip|postal|postcode|phone|tel|fax|iban|bic|ust|uid|vat|ean|gtin|isbn|code|nummer|nr|no|id|pk|fk|key|ref|rid|uuid|guid|hash)($|[_-])")

(def title-attr-re
  #"(?i)^(name|title|titel|label|bezeichnung|betreff|subject|display.?name)$")

(def temporal-attr-re
  #"(?i)(^|[_-])(datum|date|zeit|time|stamp|erstellt|geaendert|geändert|created|updated|modified|faellig|fällig|am|seit)($|[_-])")

(def outs-of tio/outputs-of)

(def ins-of tio/inputs-of)

(defn type-ref
  [schema x]
  (or (some-> (or (get-in schema [:object-types x])
                  (when (keyword? x) (get-in schema [:object-types (subs (str x) 1)])))
              :object-type-name ->name)
      (some-> (get-in schema [:interfaces x]) :interface-name ->name)
      (->name x)))

(defn action-log-types
  [schema]
  (into #{} (for [[an a] (:actions schema)
                  :let [cfg (:action-log a)]
                  :when cfg]
              (or (some->> (when (map? cfg) (:object-type cfg)) (type-ref schema))
                  (str (->name an) "-log")))))

(defn user-types
  [schema]
  (let [logs (action-log-types schema)]
    (for [[nm spec] (section-entries (:object-types schema) :object-type-name)
          :when (not (or (= :hidden (->kw (:visibility spec)))
                         (str/starts-with? (str (:plural spec)) "[LOG]")
                         (contains? logs nm)))]
      [nm spec])))

(defn attr-hidden? [a] (= :hidden (->kw (get-in a [:opts :visibility]))))

(defn link-ends
  [schema]
  (for [[k l] (:link-types schema)]
    {:link  (or (some-> (:link-type-name l) ->name) (->name k))
     :from  (type-ref schema (:from-object-type l))
     :to    (type-ref schema (:to-object-type l))
     :card  (->kw (:cardinality l))
     :spec  l}))

(defn implementers
  [schema]
  (reduce (fn [m [nm spec]]
            (reduce #(update %1 (type-ref schema %2) (fnil conj #{}) nm) m (:implements spec)))
          {} (section-entries (:object-types schema) :object-type-name)))

(defn action-managed-types
  [schema]
  (into #{}
        (comp (mapcat (comp impact/edit-targets val))
              (filter #(or (keyword? %) (string? %)))
              (map #(type-ref schema %)))
        (:actions schema)))

(defn producers-of
  [schema ot]
  (sort (for [[_ t] (:transforms schema)
              o (outs-of t)
              :when (and (= :object-type (:kind o)) (= ot (:object-type o)))]
          (->name (:transform-name t)))))

(def identifier-re
  #"(?i)(^|[_-])(id|nr|no|key|code|uuid|guid|pk)$|nummer$|nr$")

(defn title-key-of [spec]
  (some-> (or (:title-key spec) (:title spec)) ->name))

(defn key-title?
  [spec tk]
  (let [mk (:match-keys spec)]
    (or (= tk (some-> (:primary-key spec) ->name))
        (and (= 1 (count mk)) (= tk (->name (first mk)))))))

(defn readable-attr?
  [spec a]
  (let [an (->name (:attribute-name a))
        t  (->kw (:attribute-type a))]
    (and an
         (#{:string :text} t)
         (not (re-find code-attr-re an))
         (not (re-find identifier-re an))
         (not (re-find temporal-attr-re an))
         (not (attr-hidden? a))
         (not (get-in a [:opts :fk]))
         (not (get-in a [:opts :constraints :one-of]))
         (not (some #{an} (map ->name (:match-keys spec)))))))

(def per-period-re
  #"(?i)[_-](monat|tag|woche|quartal|jahr|month|day|week|quarter|year)$|[_-]per[_-]")

(def status-name-re
  #"(?i)(status|stand|art|typ|type)$")

(def attribute-priority-limit
  15)

(defn type-labels
  [nm spec]
  (->> (concat [(:display-name spec) (:plural spec) (str/replace nm #"[-_]" " ")]
               (:aliases spec))
       (keep #(some-> % str str/trim str/lower-case not-empty))
       set))

(defn override-findings
  [vocab schema severity]
  (for [[an a] (sort-by (comp str key) (:actions schema))
        :when  (map? a)
        {s :severity m :message} (spec/override-problems vocab a)
        :when  (= severity s)]
    {:entity (->name an) :entity-class :actions
     :message (str "Action " (pr-str (->name an)) ": " m)}))

