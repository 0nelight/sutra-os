(ns sutra.types.scaffold
  (:require [clojure.string :as str]
            [sutra.types.languages :as langs]))

(def ^:private type-id #"[a-z][a-z0-9-]*")

(def ^:private property-id #"[A-Za-z][A-Za-z0-9_-]*")

(defn- text [x] (str/trim (str (some-> x name))))

(defn object-type-problem
  [{:keys [name plural properties title]} create?]
  (let [nm    (text name)
        ids   (filterv seq (map text properties))
        title (text title)]
    (some-> (cond
              (str/blank? nm)
              :refusal.scaffold/name-required
              (and create? (not (re-matches type-id nm)))
              :refusal.scaffold/object-type-id-invalid
              (str/blank? (str plural))
              :refusal.scaffold/plural-required
              (and create? (empty? ids))
              :refusal.scaffold/object-type-needs-property
              (some #(not (re-matches property-id %)) ids)
              :refusal.scaffold/property-id-invalid
              (not= (count ids) (count (distinct ids)))
              :refusal.scaffold/property-ids-unique
              (and create? (str/blank? title))
              :refusal.scaffold/object-type-needs-title
              (and (seq title) (not (some #{title} ids)))
              :refusal.scaffold/title-not-a-property)
            (vector {}))))

(defn inferred-fk-side
  [card]
  (case (some-> card text not-empty keyword)
    :many-to-one :from
    :one-to-many :to
    nil))

(defn- backing-problem
  [card {:keys [strategy fk-side fk-attribute dataset datasource table query
                from-key-column to-key-column]}]
  (let [m-m? (= :many-to-many card)]
    (case strategy
      (:polymorphic :resolve-by-shared-property) nil
      :endpoint-fk (when-not (and (not m-m?) (#{:from :to} fk-side) fk-attribute)
                     (if m-m? :refusal.scaffold/link-needs-join :refusal.scaffold/link-needs-fk))
      (if m-m?
        (when-not (and (or dataset (and datasource (or (seq table) (seq query))))
                       from-key-column to-key-column)
          :refusal.scaffold/link-needs-join)
        :refusal.scaffold/link-needs-fk))))

(defn link-type-problem
  [{:keys [name from to cardinality external-source]} create?]
  (let [card (some-> cardinality text not-empty keyword)]
    (some-> (cond
              (not (every? (comp seq text) [name from to]))
              :refusal.scaffold/link-needs-name-from-to
              (and create? card) (backing-problem card external-source))
            (vector {}))))

(defn shared-property-problem
  [{:keys [type]}]
  (when (str/blank? (text type))
    [:refusal.scaffold/shared-property-needs-type {}]))

(defn- languages [function?]
  (cond->> langs/compute-language-names function? (filterv langs/code-language?)))

(defn transform-problem
  [{:keys [lifecycle language inputs outputs primary-key from-key to-key]}]
  (let [function? (= :function lifecycle)
        allowed   (languages function?)]
    (cond
      (not (some #{language} allowed))
      [(if function? :refusal.scaffold/function-language :refusal.scaffold/transform-language)
       {:language language :allowed (str/join ", " (map name allowed))}]

      (empty? inputs) [:refusal.scaffold/needs-input {}]

      (and function? (seq outputs))
      [:refusal.scaffold/function-has-no-output {:given (str/join "/" (map name outputs))}]

      function? nil

      (> (count outputs) 1) [:refusal.scaffold/one-output {:count (count outputs)}]

      (empty? outputs) [:refusal.scaffold/needs-output {}]

      (and (= :link-type (first outputs)) (not (and from-key to-key)))
      [:refusal.scaffold/link-output-needs-keys {}]

      (and (= :object-type (first outputs)) (nil? primary-key))
      [:refusal.scaffold/object-output-needs-pk {}])))

(def edit-function-stub
  "(defn edit [ctx]\n  ;; scaffold — fill in via `function edit`\n  [])\n")

(defn edit-function-problem
  [{:keys [target-type]}]
  (when (str/blank? (text target-type))
    [:refusal.scaffold/edit-function-needs-target {}]))

(defn edit-function
  [{:keys [target-type language source param-types]}]
  (cond-> {:language    (keyword (or language :clojure))
           :source      (or source edit-function-stub)
           :target-type (keyword target-type)}
    (some? param-types) (assoc :param-types param-types)))
