(ns sutra.ref.fixture)

(def kunde "ri.sutra.main.object-type.kunde")
(def auftrag "ri.sutra.main.object-type.auftrag")

(def world
  {:object-types
   {kunde   {:object-type-name :kunde
             :object-key {:properties [:nr] :source :primary-key}
             :attribute-list [{:attribute-name :nr :attribute-type :string}
                              {:attribute-name :name :attribute-type :string
                               :opts {:constraints {:max-length 5}}}
                              {:attribute-name :menge :attribute-type :integer}
                              {:attribute-name :tag :attribute-type :date}]}
    auftrag {:object-type-name :auftrag
             :attribute-list [{:attribute-name :titel :attribute-type :string}]}}
   :link-types
   {:kunde-auftrag {:cardinality :many-to-one}
    :paar          {:cardinality :many-to-many}}
   :actions {}})

(def now (java.time.Instant/parse "2026-10-05T22:30:00Z"))

(defn sigma
  ([] (sigma {}))
  ([objects]
   (let [n (atom 0)]
     {:object      (fn [T id] (get objects [T id]))
      :lookup      (fn [_W T {[k v] :key id :id}]
                     (if id
                       (keep identity [(get objects [T id])])
                       (filterv #(= v (get-in % [:values k]))
                                (keep (fn [[[t _] o]] (when (= t T) o)) objects))))
      :link-ends   (fn [_W _L _id] nil)
      :fresh-uuid  (fn [] (str "u-" (swap! n inc)))
      :now         (constantly now)
      :zone        (constantly (java.time.ZoneId/of "Europe/Vienna"))
      :username    (constantly "alma")})))

(defn with-action [w nm action]
  (assoc-in w [:actions nm] action))
