(ns ^:unit platform.product-marketplace-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.access.federation :as fed]
            [sutra.catalog.project-scope :as project-scope]
            [sutra.parser.product :as product]
            [support.mem :as mem])
  (:import (java.time Instant)))

(deftest versions-and-ranges
  (is (product/satisfies? "1.4.2" "^1.2"))
  (is (not (product/satisfies? "2.0.0" "^1.2")) "a caret never crosses a major")
  (is (not (product/satisfies? "0.4.0" "^0.3")) "below 1.0 a minor step breaks")
  (is (product/satisfies? "1.2.9" "~1.2.3"))
  (is (not (product/satisfies? "1.3.0" "~1.2.3")))
  (is (product/satisfies? "1.5.0" ">=1.0 <2"))
  (is (product/satisfies? "1.7.1" "1.x"))
  (is (not (product/satisfies? "2.0.0" "1.x")))
  (is (product/satisfies? "3.1.0" "*"))
  (is (neg? (product/compare-versions "1.9.0" "1.10.0")) "versions compare by number"))

(def ^:private releases
  [{:product "x" :version "1.1.0" :state :released :channel :stable}
   {:product "x" :version "1.2.0" :state :released :channel :dev}
   {:product "x" :version "1.3.0" :state :withdrawn :channel :stable}
   {:product "x" :version "2.0.0" :state :released :channel :stable}])

(deftest an-installation-follows-its-channel
  (testing "a stable installation sees stable releases only"
    (is (= "2.0.0" (:version (product/upgrade-target {:version "1.0.0" :channel :stable}
                                                     releases false)))))
  (testing "an automatic upgrade never crosses a breaking version"
    (is (= "1.1.0" (:version (product/upgrade-target {:version "1.0.0" :channel :stable}
                                                     releases true)))))
  (testing "a dev installation sees the dev release too; a withdrawn one never"
    (is (= "1.2.0" (:version (product/upgrade-target {:version "1.0.0" :channel :dev}
                                                     (butlast releases) true))))))

(deftest a-maintenance-window
  (let [w {:days ["sat"] :from "22:00" :to "04:00" :zone "UTC"}]
    (is (product/in-window? w (Instant/parse "2026-11-14T23:00:00Z")) "Saturday night")
    (is (product/in-window? w (Instant/parse "2026-11-15T02:00:00Z")) "past midnight")
    (is (not (product/in-window? w (Instant/parse "2026-11-15T23:00:00Z"))) "not Sunday")
    (is (product/in-window? nil (Instant/now)) "no window: any time"))
  (is (= [{:problem :bad-time :from "25"}]
         (product/window-problems {:from "25" :to "04:00"}))))

(deftest linked-products-resolve
  (let [rs [{:product "core" :version "1.4.0" :state :released :channel :stable}
            {:product "core" :version "2.0.0" :state :released :channel :stable}]]
    (is (= [{:product "core" :range "^1.2" :via :installation :install-id "i1"
             :version "1.3.0"}]
           (product/resolve-requires {"core" "^1.2"}
                                     [{:product "core" :version "1.3.0" :install-id "i1"}]
                                     rs)))
    (is (= :install (:via (first (product/resolve-requires {"core" "^1.2"} [] rs)))))
    (is (= "1.4.0" (:version (first (product/resolve-requires {"core" "^1.2"} [] rs)))))
    (is (= :unmet (:via (first (product/resolve-requires {"core" "^3"} [] rs)))))))

(def ^:private exports
  [{:project "vertrag"
    :ent {:schema {:projects {:vertrag {:description "Verträge"}}
                   :objects  {:vertrag {:description "ein Vertrag" :project :vertrag}
                              :frist   {:description "eine Frist" :project :vertrag}}
                   :links    [[:frist-vertrag :frist :vertrag :many-to-one]]}}
    :units []}])

(deftest a-bundle-is-sealed-and-checked
  (let [home   (mem/store)
        other  (mem/store)
        b      (product/bundle-of {:product :vertraege :version "1.0.0" :exports exports
                                   :inputs [{:section :object-types :name "kunde"}]
                                   :requires {:core "^1"}})
        sealed (product/sealed b {:issuer "home" :sign #(fed/sign-payload home %)})
        checks (fn [trusted] {:trusted-as #(get trusted %) :valid? fed/signature-valid?})
        known  (checks {(:public-key (fed/identity-of home)) "home"})]
    (is (= (:fingerprint (fed/identity-of home)) (get-in sealed [:signature :iss])))
    (is (= [] (product/bundle-problems sealed known)))
    (is (= [{:problem :digest-mismatch}]
           (product/bundle-problems (assoc sealed :version "9.9.9") known))
        "changed content no longer matches its digest")
    (is (= [{:problem :untrusted-issuer :issuer "home"}]
           (product/bundle-problems sealed (checks {}))))
    (is (= [{:problem :bad-signature :issuer "home"}]
           (product/bundle-problems
            (assoc-in sealed [:signature :sig] (:sig (fed/sign-payload other (:digest sealed))))
            known))
        "a signature the named key did not make")
    (is (= [{:problem :unsigned}]
           (product/bundle-problems (product/sealed b {}) {:require-signature? true})))
    (is (= [[0 :body :sources :db :secret-cipher]]
           (product/secret-paths [{:body {:sources {:db {:secret-cipher "x"}}}}]))
        "credentials are found wherever they sit")))

(deftest an-installation-shapes-the-body
  (let [b    (product/bundle-of {:product :vertraege :version "1.0.0" :exports exports})
        body (product/installed-body b {:prefix "acme" :suffix "test"
                                        :bindings {"kunde" "partner"}})]
    (is (= #{:acme-vertrag :acme-frist} (set (keys (:objects body)))))
    (is (= [:acme-frist-vertrag :acme-frist :acme-vertrag :many-to-one] (first (:links body))))
    (is (= {:vertrag-test {:description "Verträge"}} (:projects body)))
    (is (= :vertrag-test (get-in body [:objects :acme-vertrag :project])))
    (is (= ["vertrag-test"] (product/installed-projects b {:suffix "test"}))))
  (is (= [{:problem :unbound-input :section :object-types :name "kunde" :bound-to nil}]
         (product/input-problems [{:section :object-types :name "kunde"}] {}
                                 (constantly false))))
  (is (= [] (product/input-problems [{:section :object-types :name "kunde"}]
                                    {"kunde" "partner"}
                                    (fn [_ n] (= "partner" n))))))

(deftest an-input-of-any-section-binds
  (let [body {:transforms {:roh {:inputs {:k {:kind :dataset :dataset :kunden}}
                                 :output {:dataset :kunden-sauber}}}
              :objects    {:vertrag {:backing {:dataset "kunden"} :project :vertrag}}
              :actions    {:frist-setzen {:function :fristen-rechnen}}}
        bound (product/bind body {"kunden" "partner_roh" "fristen-rechnen" "fristen"}
                            [{:section :datasets :name "kunden"}
                             {:section "functions" :name "fristen-rechnen"}])]
    (is (= :partner_roh (get-in bound [:transforms :roh :inputs :k :dataset])))
    (is (= "partner_roh" (get-in bound [:objects :vertrag :backing :dataset]))
        "a string name stays a string")
    (is (= :kunden-sauber (get-in bound [:transforms :roh :output :dataset]))
        "what the bundle holds is never rebound")
    (is (= :fristen (get-in bound [:actions :frist-setzen :function])))))

(deftest documents-travel-in-the-bundle
  (let [b (product/bundle-of {:product :vertraege :version "1.0.0" :exports exports
                              :documents [{:id "d1" :project :vertrag :name "Use Case"
                                           :format :markdown :body "# Zusammenfassung"
                                           :author "wer"}]})
        [d] (product/installed-documents b "i-1" {:suffix "test"})
        [e] (product/installed-documents b "i-1" {:suffix "test"})]
    (is (= [{:id "d1" :project "vertrag" :name "Use Case" :format :markdown
             :body "# Zusammenfassung"}]
           (:documents b))
        "a document travels without who wrote it")
    (is (= "vertrag-test" (:project d)))
    (is (= (:id d) (:id e)) "an upgrade writes the same documents again")
    (is (not= "d1" (:id d)))
    (is (not= (product/digest b) (product/digest (assoc-in b [:documents 0 :body] "x")))
        "a document is part of what the digest covers")))

(deftest uninstalling-sweeps-what-it-installed
  (let [i      {:projects ["vertrag"]
                :release  {:projects {:vertrag {}}
                           :objects  {:vertrag {:project :vertrag}}
                           :links    [[:frist-vertrag :frist :vertrag :many-to-one]]}}
        schema {:projects     {"ri.p.v" {:project-name :vertrag}}
                :object-types {"ri.o.v" {:object-type-name :vertrag :project "ri.p.v"}}}]
    (is (= {:objects #{"vertrag"} :links #{"frist-vertrag"}} (product/uninstall-sweep i))
        "what it installed goes; its project is not swept")
    (is (product/sweep-standing? schema (product/uninstall-sweep i)))
    (is (not (product/sweep-standing? (dissoc schema :object-types)
                                      (product/uninstall-sweep i)))
        "a removal is seen committed once nothing it swept stands")))

(deftest a-locked-project-refuses-a-change
  (let [schema {:projects {"ri.p.v" {:project-name :vertrag}}}
        rows   [{:section "object-types" :name "vertrag" :project "ri.p.v"}]]
    (is (= "vertrag" (:project (project-scope/locked-hit schema #{"vertrag"} rows))))
    (is (nil? (project-scope/locked-hit schema #{"andere"} rows)))
    (is (nil? (project-scope/locked-hit schema #{} rows)))))
