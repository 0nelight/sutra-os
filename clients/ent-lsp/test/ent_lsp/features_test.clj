(ns ent-lsp.features-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [ent-lsp.features :as feat]
            [ent-lsp.fixture :refer [analyzed pos-of]]
            [ent-lsp.grammar :as grammar]
            [ent-lsp.workspace :as ws]))

(def uri-of #(str "file:///ws/" %))

(deftest hover-names-owner-and-file
  (let [w @analyzed
        [l c] (pos-of w "verkauf/angebot/object.ent" ":partner :many-to-one")
        h (get-in (feat/hover w "verkauf/angebot/object.ent" l c) [:contents :value])]
    (is (str/includes? h "**object** `:partner`"))
    (is (str/includes? h "Projekt `:stamm`"))
    (is (not (str/includes? h "Ordner")))
    (is (str/includes? h "stamm/partner/object.ent:3"))
    (is (str/includes? h "Geschäftspartner"))))

(deftest definition-crosses-files
  (let [w @analyzed
        [l c] (pos-of w "verkauf/angebot/actions.ent" ":partner}")]
    (is (= [{:uri "file:///ws/stamm/partner/object.ent"
             :range {:start {:line 2 :character 3} :end {:line 2 :character 11}}}]
           (feat/definition w uri-of "verkauf/angebot/actions.ent" l c)))))

(deftest references-from-the-definition
  (let [w @analyzed
        [l c] (pos-of w "stamm/partner/object.ent" ":partner")
        files (set (map :uri (feat/references w uri-of "stamm/partner/object.ent" l c false)))]
    (is (= #{"file:///ws/verkauf/angebot/object.ent" "file:///ws/verkauf/angebot/actions.ent"
             "file:///ws/verkauf/links.ent" "file:///ws/stamm/partner/actions.ent"
             "file:///ws/stamm/anbindung/erp/adr.ent"
             "file:///ws/stamm/project.ent"}
           files))))

(deftest completion-prefers-what-the-slot-wants
  (let [w (-> @analyzed
              (assoc-in [:files "verkauf/angebot/actions.ent" :text]
                        "{:schema {:actions {:x {:rules [{:object-type :pa"))
        items (:items (feat/completion w "verkauf/angebot/actions.ent" 0 50))
        top   (first (sort-by :sortText (filter #(str/starts-with? (:label %) ":pa") items)))]
    (is (= ":partner" (:label top)))
    (is (= "object · :stamm" (:detail top)))
    (is (= {:start {:line 0 :character 47} :end {:line 0 :character 50}}
           (get-in top [:textEdit :range])))))

(deftest outline-groups-by-section
  (let [syms (feat/document-symbols @analyzed "verkauf/angebot/object.ent")]
    (is (= [":objects" ":links"] (map :name syms)))
    (is (= [":angebot-partner" ":angebot-intern" ":angebot-lieferung"]
           (map :name (:children (second syms)))))))

(defn- diag [w file rule]
  (some #(when (= (name rule) (:code %)) %) (map feat/diagnostic (get-in w [:findings file]))))

(deftest quick-fix-adds-the-import-in-the-own-project
  (let [w @analyzed
        d (diag w "verkauf/anbindung/erp/ang.ent" :import-fehlt)
        d (update d :data #(into {} (map (fn [[k v]] [k (if (keyword? v) (name v) v)])) %))
        [a] (feat/code-actions w uri-of [d])]
    (is (= "Import :quellen in :verkauf ergänzen" (:title a)))
    (is (= {"file:///ws/verkauf/project.ent"
            [{:range {:start {:line 3 :character 33} :end {:line 3 :character 33}}
              :newText " :quellen"}]}
           (get-in a [:edit :changes])))))

(deftest no-quick-fix-that-closes-a-cycle
  (testing "stamm → verkauf would close verkauf → stamm"
    (let [w @analyzed
          d (diag w "stamm/partner/actions.ent" :import-fehlt)
          d (update d :data #(into {} (map (fn [[k v]] [k (if (keyword? v) (name v) v)])) %))]
      (is (some? d))
      (is (empty? (feat/code-actions w uri-of [d]))))))

(deftest property-navigation
  (let [w @analyzed
        [l c] (pos-of w "verkauf/angebot/actions.ent" ":bezeichnung")]
    (is (= ["file:///ws/verkauf/angebot/object.ent"]
           (map :uri (feat/definition w uri-of "verkauf/angebot/actions.ent" l c))))
    (let [h (get-in (feat/hover w "verkauf/angebot/actions.ent" l c) [:contents :value])]
      (is (str/includes? h "**property** `:bezeichnung` von `:angebot` · Typ `:string`")))
    (let [[l c] (pos-of w "verkauf/angebot/object.ent" ":angebot-id")]
      (is (str/includes? (get-in (feat/hover w "verkauf/angebot/object.ent" l c) [:contents :value])
                         "Primärschlüssel")))))

(deftest completion-offers-the-properties-of-the-object
  (let [w (-> @analyzed
              (assoc-in [:files "verkauf/angebot/actions.ent" :text]
                        "{:schema {:actions {:x {:rules [{:object-type :partner :set [:"))
        items (:items (feat/completion w "verkauf/angebot/actions.ent" 0 63))
        props (filter #(str/starts-with? (:detail %) "property") items)]
    (is (= #{":partner-id" ":name" ":waehrung"} (set (map :label props))))
    (is (every? #(str/starts-with? (:sortText %) "0") props))))

(deftest quick-fix-moves-a-misplaced-file
  (let [w @analyzed
        d (diag w "verkauf/notiz-kopie/object.ent" :falscher-ort)
        d (update d :data #(into {} (map (fn [[k v]] [k (if (keyword? v) (name v) v)])) %))
        [a] (feat/code-actions w uri-of "verkauf/notiz-kopie/object.ent" [d])]
    (is (= "Nach verkauf/notiz/object.ent verschieben" (:title a)))
    (is (= {:kind "rename" :oldUri "file:///ws/verkauf/notiz-kopie/object.ent"
            :newUri "file:///ws/verkauf/notiz/object.ent"}
           (dissoc (first (get-in a [:edit :documentChanges])) :options)))))

(deftest quick-fix-declares-the-project-the-folder-suggests
  (let [w @analyzed
        d (diag w "verkauf/irrlaeufer.ent" :ohne-besitzer)
        d (update d :data #(into {} (map (fn [[k v]] [k (if (keyword? v) (name v) v)])) %))
        [a] (feat/code-actions w uri-of "verkauf/irrlaeufer.ent" [d])]
    (is (= ":project :verkauf ergänzen" (:title a)))
    (is (= {"file:///ws/verkauf/irrlaeufer.ent"
            [{:range {:start {:line 2 :character 16} :end {:line 2 :character 16}}
              :newText ":project :verkauf "}]}
           (get-in a [:edit :changes])))))

(defn- plain [d] (update d :data #(into {} (map (fn [[k v]] [k (if (keyword? v) (name v) v)])) %)))

(deftest hover-shows-status-and-cardinality
  (let [w @analyzed
        [l c] (pos-of w "verkauf/angebot/object.ent" ":angebot-partner")
        h (get-in (feat/hover w "verkauf/angebot/object.ent" l c) [:contents :value])]
    (is (str/includes? h "Status `:experimental`"))
    (is (str/includes? h "`:angebot` → `:partner` · :many-to-one"))))

(deftest deprecated-reference-offers-the-replacement
  (let [w (-> @analyzed
              (ws/set-text "stamm/intern/object.ent"
                           "{:schema {:objects {:intern {:project :stamm :title :name :status :deprecated :deprecation {:reason \"alt\" :replacement :partner} :attributes [[:intern-id :string] [:name :string]]}}}}")
              ws/analyze)
        d (diag w "verkauf/links.ent" :veraltet)
        [a] (feat/code-actions w uri-of "verkauf/links.ent" [(plain d)])]
    (is (= [2] (:tags d)))
    (is (= "Durch :partner ersetzen" (:title a)))
    (is (= ":partner" (get-in a [:edit :changes "file:///ws/verkauf/links.ent" 0 :newText])))))

(deftest quick-fix-creates-a-skeleton-for-an-unknown-object
  (let [w @analyzed
        d (diag w "verkauf/angebot/object.ent" :unbekannt)
        [a] (feat/code-actions w uri-of "verkauf/angebot/object.ent" [(plain d)])
        [create edit] (get-in a [:edit :documentChanges])]
    (is (= "Object-Type :lieferung in verkauf/lieferung/object.ent anlegen" (:title a)))
    (is (= {:kind "create" :uri "file:///ws/verkauf/lieferung/object.ent"}
           (dissoc create :options)))
    (is (str/includes? (get-in edit [:edits 0 :newText]) "{:lieferung {:project    :verkauf"))
    (is (str/includes? (get-in edit [:edits 0 :newText])
                       ":attributes [[:lieferung-id :string] [:name :string]]"))))

(deftest rename-touches-definition-and-every-reference
  (let [w @analyzed
        [l c] (pos-of w "stamm/partner/object.ent" ":partner")
        edits (:changes (feat/rename w uri-of "stamm/partner/object.ent" l c "kunde"))]
    (is (contains? edits "file:///ws/stamm/partner/object.ent"))
    (is (contains? edits "file:///ws/verkauf/angebot/object.ent"))
    (is (contains? edits "file:///ws/stamm/project.ent"))
    (is (every? #(= ":kunde" (:newText %)) (apply concat (vals edits)))))
  (testing "an active resource keeps its name"
    (let [w (-> @analyzed
                (ws/set-text "stamm/intern/object.ent"
                             "{:schema {:objects {:intern {:project :stamm :status :active :title :name :attributes [[:intern-id :string] [:name :string]]}}}}")
                ws/analyze)
          [l c] (pos-of w "stamm/intern/object.ent" ":intern ")]
      (is (thrown-with-msg? Exception #"Status :active"
                            (feat/prepare-rename w "stamm/intern/object.ent" l c))))))

(deftest completion-from-the-grammar
  (let [labels (fn [text]
                 (let [w (assoc-in @analyzed [:files "verkauf/neu.ent" :text] text)
                       lines (clojure.string/split-lines text)]
                   (set (map :label
                             (:items
                              (feat/completion w "verkauf/neu.ent" (dec (count lines))
                                               (count (last lines))))))))]
    (testing "keys of an object type, without the ones already there"
      (let [ks (labels "{:schema {:objects {:a {:project :verkauf :")]
        (is (every? ks [":status" ":title" ":backing" ":implements" ":attributes"]))
        (is (not (ks ":project")))
        (is (not (ks ":partner")))))
    (testing "fixed values"
      (is (= #{":current-object" ":current-user" ":uuid" ":now"}
             (labels "{:schema {:actions {:x {:params {:p {:type :string :prefill :")))
      (is (every? (labels "{:schema {:objects {:a {:status :")
                  [":active" ":endorsed" ":deprecated"])))
    (testing "rule ops come from the surface grammar"
      (is (every? (labels "{:schema {:actions {:x {:rules [{:op :")
                  [":create" ":update" ":invoke-webhook"])))))
