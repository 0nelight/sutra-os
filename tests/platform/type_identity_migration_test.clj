(ns ^:integration platform.type-identity-migration-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.store.control.import-keys :as import-keys]
            [sutra.store.migrations.type-identity :as type-identity]
            [sutra.store.records :as r]
            [sutra.store.seat :as seat]
            [sutra.store.storage.instance-config :as ic]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(def ^:private rid "ri.sutra.main.object-type.00000000-0000-0000-0000-00000000000a")
(def ^:private schema {:object-types {rid {:object-type-name :widget}}})

(defn- store [] (ctx/store "main"))
(defn- counts [] (into {} (map (juxt :object-type :n))
                       (import-keys/import-key-counts (store) "main")))

(deftest name-keyed-rows-move-to-the-rid-once
  (import-keys/write-import-keys! (store) "main" [["widget" "\"a\"" "u1"]
                                                  ["widget" "\"b\"" "u2"]
                                                  ["weg" "\"c\"" "u3"]])
  (import-keys/write-import-keys! (store) "main" [[rid "\"b\"" "u-new"]])
  (import-keys/write-index-times! (store) "main" ["widget" "weg"] "run-alt")
  (with-redefs [seat/branch-schema (fn [_] schema)]
    (with-redefs [ic/get-cfg (fn [_ _] nil)]
      (is (= {"main" 3} (type-identity/move! (store)))))
    (is (= {rid 2 "weg" 1} (counts)) "an unresolvable name stays behind, unread")
    (is (= "u-new" (:uuid (ctl/get (store) :import-keys
                                   {:branch "main" :object-type rid :pk-value "\"b\""})))
        "what a build wrote under the RID is not overwritten")
    (let [times (import-keys/index-times (store) "main")]
      (is (inst? (get times rid)))
      (is (not (contains? times "widget"))))
    (is (nil? (type-identity/move! (store))) "once per instance")
    (is (= "true" (ic/get-cfg (store) "migrations.type-identity")))))

(deftest a-large-move-deletes-in-transactions-the-lock-table-can-hold
  (let [nm     "gross"
        rows   (mapv (fn [i] [nm (str "\"k" i "\"") (str "u" i)]) (range 1203))
        real   r/rewriting
        chunks (atom [])]
    (import-keys/write-import-keys! (store) "main" rows)
    (with-redefs [r/rewriting (fn [d rs f] (swap! chunks conj (count rs)) (real d rs f))]
      (is (= 1203 (ctl/delete-all! (store) :import-keys {:branch "main" :object-type nm}))))
    (is (<= (apply max @chunks) 500) "no commit holds more keys than the batch")
    (is (nil? (get (counts) nm)) "nothing stays behind")))
