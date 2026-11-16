(ns ^:api ontology.timeseries-property-test
  (:require [sutra.catalog.grants :as grants]
            [support.wire :refer [commit! ok? publish! uid]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.access.principals :as pr]
            [support.pod :as pod]
            [support.seed :as seed]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :once pod/with-pod-auth)

(defn- stage!
  [verb args]
  (let [r (api/mutate verb args {:stage? true})]
    (is (ok? r) (str "stage " verb ": " (pr-str (:body r))))
    r))
(defn- app-rid [schema nm]
  (some (fn [[rid s]] (when (= nm (:application-name s)) rid)) (:applications schema)))

(defn- series [spec] (tx/send! :post (paths/url :ontology-series) spec))
(defn- app-series [rid spec]
  (tx/send! :post (paths/url :my-application-series {:rid rid}) spec))

(defn- points-of
  [resp sid]
  (some (fn [s] (when (= sid (:series-id s))
                  (into {} (map vec) (:points s))))
        (get-in resp [:body :series])))

(def ^:private sync-cols
  [{:attribute-name :series_id :attribute-type :string}
   {:attribute-name :ts        :attribute-type :date}
   {:attribute-name :value     :attribute-type :decimal}])

(deftest timeseries-properties
  (let [admin-tok   (-> (api/post! :login {:username grants/admin-username
                                    :password pr/default-admin-password})
                        :body :token)
        consumer-id (uid)]
    (is (some? admin-tok) "admin bootstrap login")

    (testing "admin: a sync dataset, a type with two TSPs, and a broken sync beside them"
      (tx/with-token admin-tok
        (stage! :create-project {:name :zeit-proj
                                 :grants [{:principal consumer-id :role :ontology-viewer}]})
        (stage! :create-project {:name :geheim-proj})
        (stage! :create-project {:name :identitaet})
        (is (ok? (commit!)) "projects — every type below sits in one")

        (stage! :create-object-type
                {:name :user :project :identitaet :primary-key :user-id
                 :attributes [[:user-id :string] [:rolle :string]] :plural "Users"})
        (is (ok? (commit!)) "identity model")
        (seed/object! :user consumer-id {:user-id consumer-id :rolle "consumer"})
        (is (ok? (api/post! :register {:username "tscons" :password "tscons"
                                :user-id consumer-id})) "register consumer")

        (stage! :create-dataset {:name :zeit_serien :kind :standalone
                                 :columns sync-cols :project :zeit-proj})
        (stage! :create-dataset {:name :kaputt_serien :kind :standalone
                                 :columns (vec (butlast sync-cols))
                                 :project :zeit-proj})
        (is (ok? (commit!)) "sync datasets")

        (stage! :create-object-type
                {:name :kostenstelle
                 :project :zeit-proj
                 :title :bezeichnung
                 :primary-key :ks-nr
                 :attributes [[:ks-nr :string]
                              [:bezeichnung :string]
                              [:produktivstunden :timeseries
                               {:series {:sync :zeit_serien :rollup :sum :unit "h"}}]
                              [:saldo :timeseries {:series {:sync :zeit_serien}}]
                              [:defekt :timeseries {:series {:sync :kaputt_serien
                                                             :rollup :sum}}]]
                 :plural "Kostenstellen"})
        (stage! :create-object-type {:name :gehalt
                                     :project :geheim-proj
                                     :primary-key :mitarbeiter
                                     :attributes [[:mitarbeiter :string]
                                                  [:betrag :integer]]
                                     :plural "Gehälter"})
        (is (ok? (commit!)) "types with time-series properties")

        (stage! :create-application
                {:name :zeitvorschau :project :zeit-proj
                 :object-type :kostenstelle
                 :layout {:kind :dashboard :object-types [:kostenstelle]}})
        (stage! :create-application
                {:name :fremd :project :geheim-proj
                 :object-type :kostenstelle
                 :layout {:object-types [:kostenstelle]}})
        (is (ok? (commit!)) "apps")
        (is (ok? (publish! :zeitvorschau)) "zeitvorschau published")
        (is (ok? (publish! :fremd)) "fremd published")

        (seed/object! :kostenstelle (uid) {:ks-nr "100" :bezeichnung "Fertigung"
                                           :produktivstunden "kst-100-prod"
                                           :saldo "kst-100-saldo"})
        (seed/object! :kostenstelle (uid) {:ks-nr "900" :bezeichnung "Montage"
                                           :produktivstunden "kst-900-prod"})
        (seed/object! :kostenstelle (uid) {:ks-nr "700" :bezeichnung "Ohne Messung"})
        (seed/object! :gehalt (uid) {:mitarbeiter "Chef" :betrag 999999})
        (is (ok? (commit!)) "cost centres")

        (is (ok? (api/post! :dataset-write {:name "zeit_serien"} {:message "seed series"
                   :rows (mapv (fn [[sid d v]] {:values {:series_id sid :ts d :value v}})
                               [["kst-100-prod" "2026-08-03" 4]
                                ["kst-100-prod" "2026-08-05" 3]
                                ["kst-100-prod" "2026-08-11" 5]
                                ["kst-100-prod" "2026-01-01" 8]
                                ["kst-900-prod" "2026-08-05" 2]
                                ["kst-100-saldo" "2026-08-03" 1000]
                                ["kst-100-saldo" "2026-08-05" 1200]])}))
            "sync rows")))

    (let [schema   (tx/with-token admin-tok (:body (tx/send! :get (paths/url :schema))))
          zeit     (app-rid schema :zeitvorschau)
          fremd    (app-rid schema :fremd)
          cons-tok (-> (api/post! :login {:username "tscons" :password "tscons"}) :body :token)]
      (is (some? zeit) "app RID")
      (is (some? cons-tok) "consumer login")

      (testing "the ontology door: hours per ISO week, per series, folded by the declared rollup"
        (tx/with-token admin-tok
          (let [resp (series {:find :kostenstelle :property :produktivstunden
                              :bucket :week})]
            (is (ok? resp) (str "series refused: " (get-in resp [:body :error])))
            (is (= "h" (get-in resp [:body :unit])) "the unit rides along for the axis")
            (is (= "sum" (str (name (get-in resp [:body :rollup]))))
                "the property's own rollup, not one the request had to name")
            (let [p (points-of resp "kst-100-prod")]
              (is (== 7 (get p "2026-W32")) "4h Mon + 3h Wed, folded into one bucket")
              (is (== 5 (get p "2026-W33")) "the following ISO week")
              (is (== 8 (get p "2026-W01"))
                  "2026-01-01 is a Thursday — ISO 2026-W01, the same label the aggregate gives it")
              (is (= 3 (count p))))
            (is (== 2 (get (points-of resp "kst-900-prod") "2026-W32"))
                "the second cost centre is its own series")
            (is (= 2 (count (get-in resp [:body :series])))
                "the cost centre carrying no series id contributes none")
            (is (= 3 (get-in resp [:body :objects]))
                "…but it IS counted: 'no measurements yet' is not an invisible row"))))

      (testing "a RAW read returns the stored points, unfolded"
        (tx/with-token admin-tok
          (let [resp (series {:find :kostenstelle :property :produktivstunden})]
            (is (ok? resp))
            (is (nil? (get-in resp [:body :rollup])) "nothing is folded")
            (is (= 4 (count (:points (first (get-in resp [:body :series])))))
                "all four stored points, not three buckets"))))

      (testing "the WINDOW is half-open — [from, until)"
        (tx/with-token admin-tok
          (let [resp (series {:find :kostenstelle :property :produktivstunden
                              :from "2026-08-01" :until "2026-08-11" :bucket :week})
                p    (points-of resp "kst-100-prod")]
            (is (ok? resp))
            (is (== 7 (get p "2026-W32")) "the two August days inside the window")
            (is (nil? (get p "2026-W33"))
                "2026-08-11 is the :until bound itself — excluded, which is what half-open means")
            (is (nil? (get p "2026-W01")) "and January is before :from"))))

      (testing "the object set SCOPES the series — a filter that matches nothing answers EMPTY"
        (tx/with-token admin-tok
          (let [one (series {:find :kostenstelle :where {:ks-nr {:= "900"}}
                             :property :produktivstunden :bucket :week})]
            (is (ok? one))
            (is (= 1 (count (get-in one [:body :series]))) "only the matched cost centre")
            (is (nil? (points-of one "kst-100-prod")) "the other series is not in the answer"))
          (let [none (series {:find :kostenstelle :where {:ks-nr {:= "kein"}}
                              :property :produktivstunden :bucket :week})]
            (is (ok? none))
            (is (= [] (vec (get-in none [:body :series])))
                "no resolvable object ⇒ no series. Falling back to the whole sync would
                 turn a filter the caller could not satisfy into a read of everything")
            (is (= 0 (get-in none [:body :points]))))))

      (testing "the series entries carry their OBJECTS, labelled by the type's title key"
        (tx/with-token admin-tok
          (let [resp (series {:find :kostenstelle :property :produktivstunden :bucket :week})
                objs (some (fn [s] (when (= "kst-900-prod" (:series-id s)) (:objects s)))
                           (get-in resp [:body :series]))]
            (is (= 1 (count objs)) "a series id is a join key, not a legend")
            (is (= "Montage" (:label (first objs)))
                "the title key labels the line, so a caller without the rows can still name it"))))

      (testing "REFUSALS say what is wrong instead of answering empty"
        (tx/with-token admin-tok
          (let [r (series {:find :kostenstelle :property :saldo :bucket :month})]
            (is (= 400 (:status r)) "a bucketed read of a property with no rollup")
            (is (re-find #"no default" (get-in r [:body :error]))))
          (is (ok? (series {:find :kostenstelle :property :saldo :bucket :month
                            :rollup :max}))
              "…and naming one in the request is enough")
          (let [r (series {:find :kostenstelle :property :bezeichnung :bucket :week})]
            (is (= 400 (:status r)) "an ordinary property is not a series")
            (is (re-find #"timeseries" (get-in r [:body :error]))))
          (let [r (series {:find :kostenstelle :property :defekt :bucket :week})]
            (is (= 400 (:status r))
                "a sync missing the value column is UNANSWERABLE, not empty")
            (is (re-find #"value" (get-in r [:body :error]))))
          (let [r (series {:find :kostenstelle :property :gibtsnicht})]
            (is (= 400 (:status r)) "an unknown property"))))

      (testing "the app door answers the same question under the app's grant"
        (tx/with-token cons-tok
          (let [resp (app-series zeit {:find :kostenstelle :property :produktivstunden
                                       :bucket :week})]
            (is (ok? resp) (str "app series refused: " (get-in resp [:body :error])))
            (is (== 7 (get (points-of resp "kst-100-prod") "2026-W32")))
            (is (nil? (get-in resp [:body :scenario])) "no scenario asked, none reported"))))

      (testing "the app's SCOPE is its own layout, and a sync is never nameable"
        (tx/with-token cons-tok
          (is (= 404 (:status (app-series zeit {:find :gehalt :property :produktivstunden})))
              "a type the app does not declare is unreachable")
          (is (= 404 (:status (app-series zeit {:find :gibtsnicht :property :x})))
              "…and an unknown type answers the same, so an app can't probe the ontology")
          (is (= 400 (:status (app-series zeit {:find :kostenstelle
                                                :property :produktivstunden
                                                :around [:whatever]})))
              ":around leaves the declared scope — a 400, not a silently dropped key")
          (is (= 400 (:status (app-series zeit {:find :kostenstelle :bucket :week})))
              ":property is required — the row read is my-application")
          (is (= 404 (:status (app-series fremd {:find :kostenstelle
                                                 :property :produktivstunden})))
              "an app the consumer was never granted is 404 here too"))))))
