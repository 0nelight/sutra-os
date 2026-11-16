(ns ^:unit platform.cloudevents-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [sutra.sources.cloudevents :as ce]
            [sutra.sources.http-connector :as fhttp]))

(def ^:private attrs
  (ce/attributes {:type "com.sutra.actions.webhook" :source-name :glpi :webhook-id :notify
                  :id "outbox-17" :time "2026-11-11T08:00:00Z"}))

(deftest headers-are-binary-content-mode
  (is (= {"ce-specversion" "1.0" "ce-id" "outbox-17" "ce-source" "/sources/glpi"
          "ce-type" "com.sutra.actions.webhook" "ce-time" "2026-11-11T08:00:00Z"
          "ce-subject" "notify"}
         (ce/headers attrs))))

(deftest an-event-names-a-known-type-and-an-id
  (is (thrown? clojure.lang.ExceptionInfo (ce/headers (assoc attrs :type "com.other.x"))))
  (is (thrown? clojure.lang.ExceptionInfo (ce/headers (assoc attrs :id "")))))

(deftest the-source-is-a-uri-reference
  (is (= "/sources/a_b" (:source (ce/attributes {:source-name "a b" :id "1" :time "t"})))))

(deftest a-webhook-carries-the-event-and-keeps-its-body
  (let [seen (atom nil)
        decl {:base-url   "https://api.example"
              :operations {:notify {:method :post :path-template "/notify/{id}" :params [:id]
                                    :body-template {:id "{id}"}}}}
        src  (fhttp/->RestSource {:egress ["api.example:443"]} decl
                                 (fn [req] (reset! seen req) {:status 202 :body ""}))]
    (fhttp/invoke src :notify {:id 7} {:idempotency-key "outbox-17" :cloudevent attrs})
    (testing "a retry of the same delivery has the same ce-id: the outbox key"
      (is (= "outbox-17" (get-in @seen [:headers "ce-id"])))
      (is (= "outbox-17" (get-in @seen [:headers "Idempotency-Key"]))))
    (is (= "application/json" (get-in @seen [:headers "Content-Type"])))
    (is (= {"id" 7} (json/read-str (:body @seen))) "the body is what it always was")))

(deftest the-schema-lists-every-type
  (let [schema (json/read-str (slurp (io/file "boundaries/events/cloudevent.schema.json")))]
    (is (= (set (keys ce/types))
           (set (get-in schema ["properties" "type" "enum"]))))))
