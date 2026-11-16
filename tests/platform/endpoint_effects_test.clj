(ns ^:unit platform.endpoint-effects-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.access.http.auth :as auth]
            [sutra.store.read-only :as read-only]
            [sutra.types.paths :as paths]
            [support.registry :as registry]))

(deftest every-method-declares-its-effects
  (doseq [[id r] paths/endpoints]
    (registry/complete (update-keys (:methods r) #(vector id %)) [:effects])))

(deftest effects-are-not-the-http-method
  (testing "ein WebSocket, dessen Nachrichten das Protokoll eines Dokuments schreiben"
    (is (= paths/writes (paths/effects :document-live :get))))
  (testing "ein WebSocket, der nur beobachtet"
    (is (= paths/reads (paths/effects :ontology-watch :get))))
  (testing "POSTs, die nur fragen"
    (doseq [id [:my-application-aggregate :my-application-objects :my-application-function
                :my-application-search :my-application-facets :my-application-users
                :my-application-around :my-application-edit-history
                :my-application-data-freshness :my-application-series
                :my-application-resources :my-workspace-around :object-view-standard
                :ontology-query :ontology-series :datasource-query :writeback-preview
                :trace-explain]]
      (is (= paths/reads (paths/effects id :post)) id)))
  (testing "eine Vorschau, die ihren Entwurf ablegt, schreibt"
    (is (= paths/writes (paths/effects :application-preview :post))))
  (testing "HEAD ist das GET der Route"
    (is (= (paths/effects :view-schemes :get) (paths/effects :view-schemes :head))))
  (testing "eine Route mit lesender und schreibender Methode"
    (is (= paths/reads (paths/effects :view-schemes :get)))
    (is (= paths/writes (paths/effects :view-schemes :post)))))

(deftest bound-methods-are-the-declared-methods
  (is (= [[:view-schemes :put :undeclared] [:view-schemes :post :unbound]]
         (auth/methods-mismatch {:view-schemes {:get identity :put identity}})))
  (is (thrown? clojure.lang.ExceptionInfo
               (auth/gate-bindings {:view-schemes {:put identity}}))))

(defn- store-seen
  [id method]
  (let [seen (atom nil)]
    (#'auth/serving (fn [req] (reset! seen (:sutra/store req)) {:status 200})
                    {}
                    {:request-method method :sutra/store {:branch "main"}
                     :sutra/endpoint id})
    @seen))

(deftest the-store-follows-the-effects
  (is (read-only/read-only? (store-seen :my-application-aggregate :post)))
  (is (not (read-only/read-only? (store-seen :document-live :get))))
  (is (read-only/read-only? (store-seen :view-schemes :get)))
  (is (not (read-only/read-only? (store-seen :view-schemes :post))))
  (testing "eine Anfrage ohne Endpunkt liest nur"
    (is (read-only/read-only? (store-seen nil :post)))))
