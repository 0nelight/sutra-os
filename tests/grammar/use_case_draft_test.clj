(ns grammar.use-case-draft-test
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.test :refer [deftest is testing]]
            [sutra.use-case.draft :as draft]))

(def ^:private golden-file "tests/grammar/use_case_draft_golden.edn")

(def sample
  {:project :demo
   :group   :vorgaenge
   :object-model
   {:title "Demo"
    :types [{:id :kunde :kind :core :domain "shop" :pk "kunde-id" :source :erp}
            {:id :lieferant :kind :core :pk "lieferant-id"}
            {:id :lager :kind :core-internal :pk "lager-id"}
            {:id :umsatz :kind :derived :pk "umsatz-id"}
            {:id :auftrag :kind :use-case :pk "auftrag-id" :attributes [[:betrag :double]]}
            {:id :vorgang :kind :use-case :pk "Vorgang Nr"}
            {:id :schatten :kind :use-case :pk "kunde-id"}
            {:id :altlast :kind :retired}
            {:id :weg :kind :retired}
            {:id :bericht :kind :report}
            {:id :farbe :kind :property-of :of :auftrag}
            {:id :ohne-klasse :pk "x-id"}]
    :links [{:from :auftrag :to :kunde :card "N:1" :label "für"}
            {:from :auftrag :to :lager :card "N:M"}
            {:from :umsatz :to :kunde :style :dashed}
            {:from :vorgang :to :auftrag :card "1:N" :link-type :vorgang-auftrag}
            {:from :kunde :to :fehlt :card "N:1"}]}
   :statecharts
   [{:id :auftrag-status :type :auftrag :property :status
     :states [{:id :neu :kind :initial :label "neu"}
              {:id :offen :label "offen" :tone :warn}
              {:id :erledigt :kind :final :label "erledigt" :tone :ok}]
     :transitions [{:from :neu :to :offen :action :auftrag-oeffnen :role "Verkauf"}
                   {:from :offen :to :erledigt :action :auftrag-schliessen :guard "bezahlt"}
                   {:from :offen :to :neu :label "zurück"}]}]
   :sources    {:erp {:type :jdbc} :neu-quelle {:type :ssh}}
   :writebacks [{:action :auftrag-schliessen :system "ERP" :via :webhook}
                {:action :fehlt :system "UCS" :via :hand}]})

(def live
  {:object-types {:kunde   {:pk :kunde-id :attributes #{:kunde-id :name} :sourced? true
                            :reads #{:auftrag}}
                  :lieferant {:pk :lieferant-id :attributes #{:lieferant-id}
                              :sourced? false}
                  :lager   {:pk :lager-id :attributes #{:lager-id} :sourced? false}
                  :umsatz  {:pk :umsatz-id :attributes #{:umsatz-id} :transform? false
                            :edited? true}
                  :auftrag {:pk :auftrag-id :attributes #{:auftrag-id}}
                  :altlast {:pk :altlast-id :attributes #{}}
                  :bericht {:pk :bericht-id :attributes #{}}
                  :protokoll {:pk :p-id :attributes #{} :log? true}}
   :link-types   {:auftrag-kunde {:from :auftrag :to :kunde :cardinality :many-to-one}}
   :actions      {:auftrag-oeffnen {:on :auftrag :webhooks? false}}
   :groups       {:vorgaenge #{:kunde}}
   :sources      #{:erp}})

(defn current []
  (let [p      (draft/plan sample live)
        p-none (draft/plan sample {})]
    {:plan           p
     :plan-no-live   p-none
     :install-schema (draft/install-schema p)
     :retirements    (draft/retirements p)
     :summary        (draft/summary p)
     :dot            (draft/object-model-dot (:object-model sample) live)
     :dot-no-live    (draft/object-model-dot (:object-model sample))
     :statechart-dot (draft/statechart-dot (draft/statechart sample :auftrag-status) live)
     :without-group  (:hints (draft/plan (dissoc sample :group) live))}))

(defn write-golden! []
  (spit golden-file (with-out-str (binding [*print-namespace-maps* false]
                                    (pp/pprint (current))))))

(deftest the-draft-plans-and-draws-as-it-did
  (let [want (edn/read-string (slurp golden-file))
        have (edn/read-string (pr-str (current)))]
    (doseq [k (sort (into (set (keys want)) (keys have)))]
      (testing (pr-str k)
        (is (= (get want k) (get have k)))))))
