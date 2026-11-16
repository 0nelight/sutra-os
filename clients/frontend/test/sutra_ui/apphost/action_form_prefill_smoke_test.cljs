(ns sutra-ui.apphost.action-form-prefill-smoke-test
  (:require [clojure.string :as str]
            [cljs.test :refer [deftest is testing async]]
            [reagent.dom.server :as rds]
            [sutra-ui.render-ctx :as rctx]
            [sutra-ui.apphost.sdk :as app]
            [sutra-ui.apphost.action-form :as af]))

(defn- expand
  [[_ form-state rt descriptor opts]]
  (@#'af/form-sync! form-state rt descriptor opts)
  (@#'af/action-form* form-state rt descriptor opts))

(def ^:private partner-rid "ri.sutra.main.object-type.d52a9bfc")

(def ^:private partner-rows
  [{:id "p1" :object-type-ref partner-rid
    :values {:name "Hangar Supply GmbH" :partner-id "hangar-supply.example"}}
   {:id "p2" :object-type-ref partner-rid :values {:name "Rotor Parts" :partner-id "rotor-parts"}}
   {:id "p3" :object-type-ref partner-rid :values {:name nil :partner-id "kabinenteile.example"}}])

(def ^:private dokument
  {:id "dok-1" :object-type-ref :eingangsdokument
   :values {:lesung-netto 500.28 :lesung-datum "2026-09-01" :lesung-nr "R-17"
            :lesung-lieferant "hangar-supply.example" :lieferant-id "p1"}})

(defn- from-dok [prop] {:from :dokument :property prop})

(def ^:private specs
  {:dokument  {:type :eingangsdokument :prefill :current-object}
   :netto     {:type :number :name "Netto" :default (from-dok :lesung-netto)}
   :datum     {:type :date :name "Datum" :default (from-dok :lesung-datum)}
   :nr        {:type :string :name "Nummer" :default (from-dok :lesung-nr)}
   :lieferant {:type partner-rid :name "Lieferant" :default (from-dok :lesung-lieferant)}
   :notiz     {:type :string :name "Notiz"}})

(def ^:private descriptor
  {:object-titles {partner-rid :name}
   :object-keys   {partner-rid [:partner-id]}})

(defn- form-opts [defaults]
  {:action :rechnung-erfassen :specs specs :object dokument
   :defaults defaults :options {:lieferant partner-rows}})

(defn- cells
  [hiccup]
  (into {} (keep (fn [x]
                   (when (and (vector? x) (> (count x) 3)
                              (keyword? (second x))
                              (instance? af/StagedCell (nth x 3)))
                     [(second x) (nth x 3)])))
        (tree-seq coll? seq hiccup)))

(defn- mount
  ([] (mount {}))
  ([defaults]
   (let [inner (af/action-form descriptor (form-opts defaults))
         now   (atom nil)
         paint (fn [] (reset! now (expand (inner descriptor (form-opts defaults)))))]
     {:render (fn [] (rds/render-to-string (paint)))
      :value  (fn [p] (paint) (deref (get (cells @now) p)))
      :type!  (fn [p v] (paint) (reset! (get (cells @now) p) v))})))

(defn- marks [html] (count (re-seq #"capp-actionform__edited" html)))

(defn- mark-title [html]
  (some-> (re-find #"capp-actionform__edited\"[^>]*title=\"([^\"]*)\"|title=\"([^\"]*)\"[^>]*capp-actionform__edited" html)
          rest (->> (some identity))))

(deftest an-untouched-prefill-is-not-marked
  (let [{:keys [render]} (mount)
        html (render)]
    (is (str/includes? html "R-17") "the reading stands in the field")
    (is (zero? (marks html)) "nothing edited, nothing marked")))

(deftest an-edited-prefill-is-marked-with-its-original
  (testing "number"
    (let [{:keys [render type!]} (mount)]
      (type! :netto 510)
      (let [html (render)]
        (is (= 1 (marks html)))
        (is (str/includes? html ">bearbeitet<"))
        (is (re-find #"Vorbelegt: 500[,.]28" (str (mark-title html))) (str (mark-title html))))
      (type! :netto 500.28)
      (is (zero? (marks (render))) "back to the reading → no longer edited")))
  (testing "string, and cleared counts as edited"
    (let [{:keys [render type!]} (mount)]
      (type! :nr "R-18")
      (is (= "Vorbelegt: R-17" (mark-title (render))))
      (type! :nr "")
      (is (= 1 (marks (render))))))
  (testing "date — compared as a day"
    (let [{:keys [render type!]} (mount)]
      (type! :datum "2026-09-02")
      (is (= "Vorbelegt: 2026-09-01" (mark-title (render))))
      (type! :datum "2026-09-01")
      (is (zero? (marks (render))))))
  (testing "object reference — the prefill is the KEY, the picker writes the id"
    (let [{:keys [render type!]} (mount)]
      (type! :lieferant "p1")
      (is (zero? (marks (render))) "the same supplier by id is not an edit")
      (type! :lieferant "p2")
      (is (= "Vorbelegt: Hangar Supply GmbH" (mark-title (render)))
          "the original reads as its title, not its key or id")))
  (testing "a field without a prefill gets nothing"
    (let [{:keys [render type!]} (mount)]
      (type! :notiz "geprüft")
      (is (zero? (marks (render)))))))

(deftest prefill-edit-compares-by-type
  (rctx/install! "de")
  (is (nil? (af/prefill-edit {:type :number} 500.28 "500,28" {})))
  (is (nil? (af/prefill-edit {:type :date} "2026-09-01" "2026-09-01T00:00:00" {})))
  (is (nil? (af/prefill-edit {:type :string} nil "neu" {})) "no prefill, no mark")
  (is (= {:original "kabinenteile.example"}
         (af/prefill-edit {:type partner-rid} "kabinenteile.example" "p2"
                           {:object? true :rows partner-rows :key-props [:partner-id]
                            :caption #(or (get-in % [:values :name])
                                          (get-in % [:values :partner-id]))}))))

(deftest an-empty-local-default-does-not-hide-the-global-prefill
  (testing "a view variable that is still nil falls through to the action's prefill"
    (let [{:keys [value render]} (mount {:lieferant nil :nr nil})]
      (is (= "hangar-supply.example" (value :lieferant)))
      (is (= "R-17" (value :nr)))
      (is (zero? (marks (render))))))
  (testing "…and so does an EMPTY object set, the shape an unset :output variable has"
    (let [{:keys [value]} (mount {:lieferant []})]
      (is (= "hangar-supply.example" (value :lieferant)))))
  (testing "a value wins over the global prefill"
    (let [{:keys [value]} (mount {:lieferant [{:id "p2" :object-type-ref partner-rid
                                               :values {:name "Rotor Parts"}}]})]
      (is (= "p2" (value :lieferant))))))

(deftest a-later-output-overrides-the-fallen-through-prefill
  (testing "through the view: `:defaults {:lieferant {:val :neu}}`, `:neu` first nil, then
            the supplier a \"+ neu\" modal created"
    (let [desc (fn [neu]
                 {:id "ri.sutra.main.application.eingangsdokument-view-full"
                  :name :eingangsdokument-view-full
                  :object dokument
                  :object-titles {partner-rid :name}
                  :object-keys   {partner-rid [:partner-id]}
                  :action-params {:rechnung-erfassen
                                  {:dokument  (:dokument specs)
                                   :lieferant (assoc (:lieferant specs)
                                                     :default (from-dok :lieferant-id))}}
                  :layout {:kind :rows
                           :vars {:partner {:from :geschaeftspartner}
                                  :neu     {:default neu}}
                           :items [{:widget :action-form :action :rechnung-erfassen
                                    :defaults {:lieferant {:val :neu}}
                                    :options {:lieferant {:val :partner}}}]}})
          data {:geschaeftspartner partner-rows}
          leer (rds/render-to-string [app/render-data-app (desc nil) data])
          neu  (rds/render-to-string
                [app/render-data-app
                 (desc [{:id "p-neu" :object-type-ref partner-rid :values {:name "Neu GmbH"}}])
                 data])]
      (is (re-find #"<option[^>]*selected[^>]*>Hangar Supply GmbH<|value=\"Hangar Supply GmbH\""
                   leer)
          "empty variable → the reading's supplier, by its title")
      (is (re-find #"<option[^>]*selected[^>]*>Neu GmbH<|value=\"Neu GmbH\"" neu)
          "the created supplier replaces it"))))

(def ^:private lesungen
  {"l1" {:netto 500.28 :datum "2026-09-01" :partner-id "hangar-supply.example"}
   "l2" {:netto 120 :datum "2026-09-05" :partner-id "rotor-parts"}})

(defn- from-lesung [prop] {:from :lesung :property prop})

(def ^:private lesung-specs
  {:lesung    {:type :beleglesung :required true :exposure :hidden}
   :dokument  {:type :eingangsdokument :prefill :current-object}
   :netto     {:type :number :name "Netto" :default (from-lesung :netto)}
   :datum     {:type :date :name "Datum" :default (from-lesung :datum)}
   :lieferant {:type partner-rid :name "Lieferant" :default (from-lesung :partner-id)}
   :notiz     {:type :string :name "Notiz"}})

(defn- fake-evaluate
  [_action values _search]
  (let [l (get lesungen (str (:lesung values)))]
    (js/Promise.resolve
     {:values (merge (when l {:netto (:netto l) :datum (:datum l) :lieferant (:partner-id l)})
                     (into {} (filter (comp some? val)) values))})))

(defn- mount-late
  []
  (let [wahl  (atom nil)
        opts  (fn [] {:action :rechnung-aus-lesung-erfassen :specs lesung-specs
                      :object dokument :evaluate fake-evaluate
                      :defaults {:dokument "dok-1" :lesung @wahl}
                      :options {:lieferant partner-rows}})
        inner (af/action-form descriptor (opts))
        now   (atom nil)
        paint (fn [] (reset! now (expand (inner descriptor (opts)))))]
    {:render (fn [] (rds/render-to-string (paint)))
     :pick!  (fn [id] (reset! wahl id) (paint))
     :value  (fn [p] (paint) (deref (get (cells @now) p)))
     :type!  (fn [p v] (paint) (reset! (get (cells @now) p) v))}))

(defn- settled
  [f] (js/setTimeout f 300))

(deftest a-prefill-from-a-late-hidden-object-param-is-marked-when-edited
  (async done
    (let [{:keys [render pick! value type!]} (mount-late)]
      (render)
      (pick! "l1")
      (settled
       (fn []
         (is (= 500.28 (value :netto)) "the reading's net amount stands in the field")
         (is (zero? (marks (render))) "untouched, nothing marked")
         (type! :netto 499.99)
         (let [html (render)]
           (is (= 1 (marks html)) "Netto edited → bearbeitet")
           (is (re-find #"Vorbelegt: 500[,.]28" (str (mark-title html))) (str (mark-title html))))
         (type! :lieferant "p1")
         (is (= 1 (marks (render))) "the same supplier by id (prefill is its key) is no edit")
         (type! :lieferant "p2")
         (is (= 2 (marks (render))))
         (type! :lieferant "p1")
         (type! :netto 500.28)
         (is (zero? (marks (render))) "back to the reading → unmarked")
         (testing "another reading re-baselines; untouched fields stay unmarked"
           (type! :netto 499.99)
           (pick! "l2")
           (settled
            (fn []
              (is (= "2026-09-05" (value :datum)) "an untouched field follows the new reading")
              (let [html (render)]
                (is (= 2 (marks html)) "only what the user holds: netto and lieferant")
                (is (re-find #"Vorbelegt: 120" html) "netto measured against the NEW reading")
                (is (re-find #"Vorbelegt: Rotor Parts" html)))
              (type! :netto 120)
              (type! :lieferant "p2")
              (is (zero? (marks (render))) "equal to the new reading → unmarked")
              (done)))))))))
