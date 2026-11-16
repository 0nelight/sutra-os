(ns support.edit-world
  (:require [support.replay :as replay]
            [support.system :as tsys]
            [clojure.set]
            [sutra.catalog.kernel :as kernel]
            [sutra.types.object-set :as os]
            [sutra.ref.objectset :as ros]))

(def objects
  {:partner [{:id "p1" :values {:partner-nr "P1" :notiz "alt"}}
             {:id "p2" :values {:partner-nr "P2" :notiz nil}}]
   :buchung [{:id "b1" :values {:buchung-nr "B1" :partner-nr "P1" :status "offen"}}
             {:id "b2" :values {:buchung-nr "B2" :partner-nr "P1" :status "bezahlt"}}
             {:id "b3" :values {:buchung-nr "B3" :partner-nr "P2" :status "offen"}}]
   :tag     [{:id "t1" :values {:tag-id "T1"}}]})

(def ^:private tag-links #{["b1" "t1"]})

(defn world [language fn-source & [extra]]
  {:object-types
   {:partner {:attribute-list [{:attribute-name :partner-nr :attribute-type :string}
                               {:attribute-name :notiz :attribute-type :string}]
              :plural "Partner"}
    :buchung {:attribute-list [{:attribute-name :buchung-nr :attribute-type :string}
                               {:attribute-name :partner-nr :attribute-type :string}
                               {:attribute-name :status :attribute-type :string}]
              :plural "Buchungen"}
    :tag     {:attribute-list [{:attribute-name :tag-id :attribute-type :string}] :plural "Tags"}}
   :link-types
   {:buchung-partner {:from-object-type :buchung :to-object-type :partner
                      :cardinality :many-to-one
                      :forward-api-name :partner :reverse-api-name :buchungen}
    :buchung-tag     {:from-object-type :buchung :to-object-type :tag
                      :cardinality :many-to-many
                      :forward-api-name :tags :reverse-api-name :getaggt}}
   :transforms
   {:partner {:transform-name :partner
              :output {:kind :object-type :object-type :partner :primary-key :partner-nr}}
    :buchung {:transform-name :buchung
              :output {:kind :object-type :object-type :buchung :primary-key :buchung-nr}}
    :tag     {:transform-name :tag
              :output {:kind :object-type :object-type :tag :primary-key :tag-id}}
    :link-buchung-partner
    {:transform-name :link-buchung-partner
     :input  {:kind :object-type :object-type :buchung}
     :output {:kind :link-type :link-type :buchung-partner :fk-attribute :partner-nr}}}
   :functions (assoc (::functions extra)
                     :edit (merge {:function-name :edit :target-type :partner
                                   :language language :source fn-source}
                                  (dissoc extra ::functions)))})

(defn- by-id [id] (some #(when (= id (:id %)) %) (apply concat (vals objects))))

(defn- type-of [id] (some (fn [[t os]] (when (some #(= id (:id %)) os) t)) objects))

(defn- linked [{:keys [link dir]} id]
  (case link
    :buchung-partner
    (if (= :from dir)
      (let [pk (get-in (by-id id) [:values :partner-nr])]
        (filter #(= pk (get-in % [:values :partner-nr])) (:buchung objects)))
      (let [pk (get-in (by-id id) [:values :partner-nr])]
        (filter #(= pk (get-in % [:values :partner-nr])) (:partner objects))))
    :buchung-tag
    (if (= :to dir)
      (keep (fn [[b t]] (when (= b id) (by-id t))) tag-links)
      (keep (fn [[b t]] (when (= t id) (by-id b))) tag-links))))

(defn- match? [obj [op a b & _more :as w]]
  (case op
    nil   true
    :and  (every? #(match? obj %) (rest w))
    :or   (some #(match? obj %) (rest w))
    :id-in (contains? (set a) (:id obj))
    :=    (= b (get-in obj [:values a]))
    :not= (not= b (get-in obj [:values a]))
    :nil? (= b (nil? (get-in obj [:values a])))))

(defn- run-ir [{:keys [op in] :as ir}]
  (case op
    :scan          (get objects (:root ir))
    :static        (map by-id (:ids ir))
    :filter        (filter #(match? % (:where ir)) (run-ir in))
    :search-around (mapcat #(linked ir (:id %)) (run-ir in))
    :intersect     (let [[a & bs] (map run-ir in)
                         keep-ids (apply clojure.set/intersection (map #(set (map :id %)) bs))]
                     (filter #(keep-ids (:id %)) a))
    :order         (run-ir in)
    :page          (cond->> (run-ir in) (:limit ir) (take (:limit ir)))))

(def reads
  (atom 0))

(defn- read-fn [ctx _io nm args]
  (swap! reads inc)
  (let [spec (os/by-name (get-in ctx [:schema :object-sets]) nm)]
    (mapv (fn [o] (assoc o :object-type-ref (type-of (:id o))))
          (run-ir (ros/instantiate (os/definition spec) (or args {}))))))

(defn- logged [res] (mapv (juxt :op :type :payload) (:ops res)))

(defn run [language source bindings & [extra rule]]
  (tsys/ensure!)
  (let [res (replay/evaluate {:verbs (merge (tsys/verbs)) :read-fn read-fn}
                             (fn [ctx io]
                               (reset! reads 0)
                               (kernel/eval-step
                                (merge {:op :apply-edit-function :function :edit} rule)
                                (world language source extra) bindings ctx io)))]
    [(logged res) res]))

(defn handle [id] (assoc (by-id id) :object-type-ref (type-of id)))

(defn fails [f] (try (f) nil (catch Exception e (ex-message e))))

