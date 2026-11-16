(ns support.array-has
  (:require [sutra.store.api :as api]))

(def mail "ri.sutra.main.object-type.00000000-0000-0000-0000-0000000000a1")

(def world
  {:object-types
   {mail {:object-type-name :mail
          :attribute-list [{:attribute-name :code :attribute-type :string}
                           {:attribute-name :anhang :attribute-type [:array :string]}
                           {:attribute-name :seiten :attribute-type [:array :integer]}]
          :plural "Mails"}}})

(def ^:private rows
  [{:code "A" :anhang ["abc" "def"] :seiten [1 2]}
   {:code "B" :anhang ["ab"] :seiten [3]}
   {:code "C" :anhang []}
   {:code "D"}])

(def cases
  [[[:has :anhang "abc"] #{"A"}]
   [[:has :anhang "ab"] #{"B"}]
   [[:has :anhang "a"] #{}]
   [[:has :seiten 3] #{"B"}]
   [[:or [:has :anhang "def"] [:= :code "D"]] #{"A" "D"}]])

(defn seed! [db]
  (let [basis (with-open [s (api/snapshot db {:branch "main"})] (api/basis s))]
    (api/transact! db
                   {:basis basis :actor {:principal "t"} :schema {:authored world :migration []}
                    :ops (map-indexed
                          (fn [i v]
                            {:op :create-object :type mail
                             :payload {:id (format "00000000-0000-0000-0000-%012d" (inc i))
                                       :values v}})
                          rows)})))

(defn codes [db where]
  (with-open [s (api/snapshot db {:branch "main"})]
    (into #{} (map (comp :code :values))
          (api/read s {:system? true}
                    [:select {:op :filter :where where :in {:op :scan :root mail}}]))))
