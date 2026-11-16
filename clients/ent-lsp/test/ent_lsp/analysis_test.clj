(ns ent-lsp.analysis-test
  (:require [clojure.test :refer [deftest is testing]]
            [ent-lsp.analysis :as an]
            [ent-lsp.fixture :refer [analyzed]]
            [ent-lsp.workspace :as ws]))

(def own-rules #{:unbekannt :import-fehlt :export-fehlt :verwaister-link
                 :doppelte-definition :falscher-ort :ohne-besitzer
                 :unbekannte-eigenschaft :doppelte-eigenschaft :pk-fehlt
                 :sync-beim-verbraucher
                 :action-sprawl :action-pro-eigenschaft})

(defn- found [w]
  (set (for [[file fs] (:findings w) f fs :when (own-rules (:rule f))]
         [(:rule f) file (get-in f [:vars :name])])))

(deftest one-finding-per-rule
  (is (= #{[:unbekannt "verkauf/angebot/object.ent" ":lieferung"]
           [:export-fehlt "verkauf/angebot/object.ent" ":intern"]
           [:export-fehlt "verkauf/links.ent" ":intern"]
           [:import-fehlt "verkauf/anbindung/erp/ang.ent" ":erp"]
           [:import-fehlt "stamm/partner/actions.ent" ":angebot"]
           [:verwaister-link "verkauf/links.ent" ":partner-intern"]
           [:doppelte-definition "stamm/notiz/object.ent" ":notiz"]
           [:doppelte-definition "verkauf/notiz-kopie/object.ent" ":notiz"]
           [:ohne-besitzer "verkauf/irrlaeufer.ent" ":irrlaeufer"]
           [:falscher-ort "verkauf/notiz-kopie/object.ent" ":notiz"]
           [:unbekannte-eigenschaft "verkauf/angebot/actions.ent" ":rabatt"]
           [:doppelte-eigenschaft "verkauf/irrlaeufer.ent" ":x"]
           [:pk-fehlt "verkauf/angebot/actions.ent" ":angebot-anlegen"]
           [:sync-beim-verbraucher "stamm/anbindung/erp/adr.ent" ":adr_raw"]
           [:sync-beim-verbraucher "verkauf/anbindung/erp/ang.ent" ":ang_raw"]
           [:action-pro-eigenschaft "stamm/partner/actions.ent" ":setze-name"]}
         (found @analyzed))))

(deftest ownership
  (let [by-key (get-in @analyzed [:index :by-key])
        owner  (fn [k n] (let [d (first (by-key [k n]))] [(:owner d) (:owner-source d)]))]
    (testing "declared or derived from the edited object type — never the folder"
      (is (= [:stamm :declared] (owner :object :intern)))
      (is (= [:verkauf :derived] (owner :action :angebot-anlegen)))
      (is (= [:stamm :derived] (owner :action :setze-name)))
      (is (= [nil nil] (owner :object :irrlaeufer)))
      (is (= :verkauf (:suggest (first (by-key [:object :irrlaeufer]))))))
    (testing "a writeback belongs to the owner of the type it writes, not to the source"
      (is (= [:stamm :derived] (owner :writeback "ADR"))))
    (testing "a source entry without :type contributes, it does not define"
      (is (= ["quellen/erp.ent"] (map :file (by-key [:source :erp])))))))

(deftest platform-grammar-is-consulted
  (let [msgs (map :message (get-in @analyzed [:findings "quellen/project.ent"]))]
    (is (some #(re-find #"disallowed key.*exports" %) msgs))))

(deftest properties-are-scoped-to-their-object
  (let [idx (:index @analyzed)]
    (testing "attributes become properties of their object"
      (is (= ["stamm/partner/object.ent"] (map :file (get-in idx [:props [:partner :name]]))))
      (is (= :stamm (:owner (first (get-in idx [:props [:partner :name]]))))))
    (testing "the same name on another object is another symbol"
      (is (not= (first (get-in idx [:props [:partner :name]]))
                (first (get-in idx [:props [:intern :name]])))))
    (testing "title, set and link foreign key resolve to properties"
      (let [targets (fn [file nm] (->> (get-in idx [:refs-by-file file])
                                       (filter #(and (:object %) (= nm (:name %))))
                                       (mapcat :targets) (map (juxt :object :name)) set))]
        (is (= #{[:angebot :bezeichnung]} (targets "verkauf/angebot/actions.ent" :bezeichnung)))
        (is (= #{[:angebot :partner-id]} (targets "verkauf/angebot/object.ent" :partner-id)))))
    (testing "the primary key is :backing :primary-key; without it the type is keyless"
      (is (= [:angebot-id] (an/primary-key (:spec @analyzed) idx :angebot)))
      (is (nil? (an/primary-key (:spec @analyzed) idx :partner))))))

(defn- with-file [w rel text]
  (ws/analyze (ws/set-text w rel text)))

(deftest import-cycles-are-no-finding
  (testing "the ontology has no load order, so imports may form a cycle"
    (let [w (-> @analyzed
                (with-file "bericht/project.ent"
                  "{:schema {:projects {:bericht {:imports [:quellen]}}}}"))
          rules (fn [file] (set (map :rule (get-in w [:findings file]))))]
      (is (not (contains? (rules "bericht/project.ent") :import-zyklus)))
      (is (not-any? #(re-find #"(?i)zykl|cycle" (str (:message %)))
                    (get-in w [:findings "bericht/project.ent"]))))))

(deftest a-project-has-no-stage
  (let [w (with-file @analyzed "auswertung/project.ent"
            "{:schema {:projects {:auswertung {:stage :workflow :imports [:verkauf]}}}}")]
    (is (some #(re-find #"stage" (str (:message %)))
              (get-in w [:findings "auswertung/project.ent"]))
        "the platform grammar refuses :stage (sutra)")))

(deftest action-sprawl-counts-actions-per-object
  (let [actions (apply str (for [i (range 11)]
                             (str ":aktion-" i
                                  " {:rules [{:op :create :object-type :notiz :set [:text]}]}\n")))
        w (with-file @analyzed "stamm/notiz/actions.ent"
            (str "{:schema {:actions {" actions "}}}"))]
    (is (some #(= :action-sprawl (:rule %)) (get-in w [:findings "stamm/notiz/object.ent"])))))
