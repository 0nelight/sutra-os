(ns ^:api platform.object-set-watch-test
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api])
  (:import (java.net URI)
           (java.net.http HttpClient WebSocket WebSocket$Listener)
           (java.util.concurrent LinkedBlockingQueue TimeUnit)))

(use-fixtures :once pod/with-mem-pod)

(def ^:private schema
  {:schema
   {:projects {:calls {}}
    :datasets
    {:call_journal
     {:project :calls
      :kind    :standalone
      :columns [{:attribute-name :call_id :attribute-type :string}
                {:attribute-name :owner :attribute-type :string}
                {:attribute-name :gone :attribute-type :boolean}]
      :stream  {:partitions 1 :primary-key {:columns [:call_id] :deletion-column :gone}}}}
    :objects
    {:call {:project    :calls
            :symbol     "📞"
            :plural     "Calls"
            :attributes [[:call-id :string] [:owner :string]]
            :backing    {:dataset :call_journal :primary-key :call-id}}}}})

(defn- open-socket!
  [messages]
  (let [text (StringBuilder.)]
    (.join
     (.buildAsync
      (.newWebSocketBuilder (HttpClient/newHttpClient))
      (URI. (str (str/replace (pod/base-url) #"^http" "ws") "/api/ontology/watch"))
      (reify WebSocket$Listener
        (onText [_ ws data last?]
          (.append text data)
          (when last?
            (.put ^LinkedBlockingQueue messages (json/read-str (str text)))
            (.setLength text 0))
          (.request ws 1)
          nil))))))

(defn- next-of
  [^LinkedBlockingQueue messages kinds ms]
  (loop []
    (when-let [m (.poll messages ms TimeUnit/MILLISECONDS)]
      (if (kinds (get m "type")) m (recur)))))

(defn- publish!
  [record]
  (is (ok? (api/post! :stream-records {:name "call_journal"} {:records [record]})))
  (is (ok? (api/post! :stream-archive {:name "call_journal"} {}))))

(defn- states
  [m]
  (mapv (juxt #(get % "state") #(get-in % ["object" "call-id"]))
        (get m "updates")))

(deftest a-filtered-subscription-hears-only-its-own-objects
  (let [r (api/post! :install schema)] (is (ok? r) (pr-str (:body r))))
  (is (ok? (api/post! :staging)))
  (publish! {:call_id "a" :owner "me" :gone false})
  (publish! {:call_id "b" :owner "other" :gone false})
  (let [messages (LinkedBlockingQueue.)
        ws       (open-socket! messages)
        changes  #{"objectSetChanged" "refreshObjectSet"}]
    (try
      (.join (.sendText ws
                        (json/write-str
                         {:id "1"
                          :requests [{:objectSet {:type "filter"
                                                  :objectSet {:type "base" :objectType "call"}
                                                  :where {:type "eq" :field "owner"
                                                          :value "me"}}}]})
                        true))
      (let [answer (next-of messages #{"subscribeResponses"} 5000)]
        (is (= ["success"] (mapv #(get % "type") (get answer "responses"))) (pr-str answer)))
      (testing "a change of someone else's object says nothing"
        (publish! {:call_id "b" :owner "other" :gone false})
        (is (nil? (next-of messages changes 1500))))
      (testing "a change of its own object arrives with the object"
        (publish! {:call_id "a" :owner "me" :gone true})
        (publish! {:call_id "a" :owner "me" :gone false})
        (let [m (next-of messages changes 5000)]
          (is (= "REMOVED" (get-in m ["updates" 0 "state"])) (pr-str m)))
        (let [m (next-of messages changes 5000)]
          (is (= [["ADDED_OR_UPDATED" "a"]] (states m)) (pr-str m))))
      (testing "an object entering the filter is added"
        (publish! {:call_id "b" :owner "me" :gone false})
        (let [m (next-of messages changes 5000)]
          (is (= [["ADDED_OR_UPDATED" "b"]] (states m)) (pr-str m))))
      (testing "an object leaving the filter is removed, then says nothing more"
        (publish! {:call_id "b" :owner "other" :gone false})
        (let [m (next-of messages changes 5000)]
          (is (= "REMOVED" (get-in m ["updates" 0 "state"])) (pr-str m)))
        (publish! {:call_id "b" :owner "other" :gone false})
        (is (nil? (next-of messages changes 1500))))
      (testing "a filter the store cannot answer is refused by name"
        (.join (.sendText ws
                          (json/write-str
                           {:id "2"
                            :requests [{:objectSet {:type "filter"
                                                    :objectSet {:type "base" :objectType "call"}
                                                    :where {:type "wildcard" :field "owner"}}}]})
                          true))
        (let [answer (next-of messages #{"subscribeResponses"} 5000)]
          (is (= "INVALID_FILTER" (get-in answer ["responses" 0 "errors" 0 "error"]))
              (pr-str answer))))
      (finally (.abort ws)))))
