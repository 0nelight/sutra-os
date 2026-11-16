(ns ^:integration platform.flaw-actions-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.core]
            [sutra.store.core.commit]
            [sutra.automation.rules :as rules]
            [sutra.automation.runs :as autos]
            [sutra.store.access.run-log :as run-log]
            [sutra.store.access.scope :as ctx]
            [sutra.access.principals :as pr]
            [sutra.types.paths :as paths]
            [support.aviation :refer [ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private ergaenzung
  {:actions
   {:mechaniker-loeschen
    {:params {:mechaniker {:name "Mechaniker" :type :mechaniker :required true}}
     :rules  [{:op :delete-object :object-type :mechaniker :target [:get :mechaniker]}]
     :edits  [{:object-type :mechaniker}]}
    :luftfahrzeug-loeschen
    {:params {:luftfahrzeug {:name "Luftfahrzeug" :type :luftfahrzeug
                             :required true}}
     :rules  [{:op :delete-object :object-type :luftfahrzeug :target [:get :luftfahrzeug]}]
     :edits  [{:object-type :luftfahrzeug}]}
    :mechaniker-umbenennen
    {:params {:mechaniker {:name "Mechaniker" :type :mechaniker :required true}
              :name       {:name "Name" :type :string :required true}}
     :rules  [{:op :update :object-type :mechaniker :target [:get :mechaniker]
               :values {:name [:get :name]}}]
     :edits  [{:object-type :mechaniker}]}}})

(defn- als-admin [f]
  (binding [tx/*auth-token* (-> (api/post! :login {:username grants/admin-username
                                                   :password pr/default-admin-password})
                                :body :token)]
    (f)))

(defn- mit-welt [f]
  (doseq [s [ontologie ergaenzung]]
    (let [r (api/post! :install {:schema s})] (assert (ok? r) (pr-str (:body r))))
    (assert (ok? (api/post! :staging))))
  (f))

(use-fixtures :once pod/with-mem-pod-auth als-admin mit-welt)

(defn- tun! [action args] (api/mutate action args {:publish? true}))

(defn- lesen [typ]
  (:rows (:body (tx/send! :post (paths/url :ontology-query) {:find typ}))))

(defn- id-von [typ k v] (some #(when (= v (get-in % [:values k])) (:id %)) (lesen typ)))

(defn- kanten [link]
  (set (map (juxt #(str (or (:from-id %) (:from_id %))) #(str (or (:to-id %) (:to_id %))))
            (:body (api/list-links {:type link})))))

(deftest row-without-log
  (testing "ein Fehler nach dem Zeilen-Write: weder Zeilen noch Log-Eintrag"
    (let [vorher  (count (run-log/action-log (ctx/store "main")))
          orig    @#'sutra.store.core.commit/apply!
          treffer (atom 0)
          r       (with-redefs-fn
                    {#'sutra.store.core.commit/apply! (fn [& args]
                                               (apply orig args)
                                               (swap! treffer inc)
                                               (throw
                                                (ex-info "injected after the commit's writes" {})))}
      #(tun! :mechaniker-anlegen {:lizenz "RWL-1" :name "Rune" :muster "A320"}))]
      (is (pos? @treffer) "der Fehler wurde nach den Schreibvorgängen des Commits eingeschleust")
      (is (not (ok? r)))
      (is (nil? (id-von :mechaniker :lizenz "RWL-1")) "keine Zeile")
      (is (= vorher (count (run-log/action-log (ctx/store "main")))) "kein Log-Eintrag"))))

(deftest dangling-link
  (testing "G2: ein gelöschtes Objekt hinterlässt keine gespeicherten Links (Quellseite)"
    (is (ok? (tun! :luftfahrzeug-registrieren {:kennzeichen "D-DLAA" :muster "A320"})))
    (is (ok? (tun! :mechaniker-anlegen {:lizenz "DL-1" :name "Dagny" :muster "A320"})))
    (is (ok? (tun! :berechtigen {:mechaniker "DL-1" :luftfahrzeug "D-DLAA"})))
    (let [m (str (id-von :mechaniker :lizenz "DL-1"))]
      (is (some #(= m (first %)) (kanten :berechtigung)) "die Kante steht")
      (is (ok? (tun! :mechaniker-loeschen {:mechaniker "DL-1"})))
      (is (nil? (id-von :mechaniker :lizenz "DL-1")))
      (is (not-any? #(= m (first %)) (kanten :berechtigung))
          "keine Kante am gelöschten Mechaniker")))
  (testing "G2: ebenso auf der Zielseite"
    (is (ok? (tun! :luftfahrzeug-registrieren {:kennzeichen "D-DLBB" :muster "A320"})))
    (is (ok? (tun! :mechaniker-anlegen {:lizenz "DL-2" :name "Detlef" :muster "A320"})))
    (is (ok? (tun! :berechtigen {:mechaniker "DL-2" :luftfahrzeug "D-DLBB"})))
    (let [l (str (id-von :luftfahrzeug :kennzeichen "D-DLBB"))]
      (is (some #(= l (second %)) (kanten :berechtigung)) "die Kante steht")
      (is (ok? (tun! :luftfahrzeug-loeschen {:luftfahrzeug "D-DLBB"})))
      (is (nil? (id-von :luftfahrzeug :kennzeichen "D-DLBB")))
      (is (not-any? #(= l (second %)) (kanten :berechtigung))
          "keine Kante am gelöschten Luftfahrzeug"))))

(defn- automation-id [nm]
  (some #(when (= nm (:name %)) (:id %)) (:automations (:body (api/get! :automations)))))

(defn- laeufe [id]
  (let [b (:body (api/get! :automation-runs {:id id}))]
    (if (map? b) (:runs b) b)))

(deftest unrecorded-run
  (testing "ein Fehler nach der Wirkung: Wirkung und Run-Protokoll, oder keins von beiden"
    (is (ok? (tun! :mechaniker-anlegen {:lizenz "UR-1" :name "Udo" :muster "A320"})))
    (is (ok? (api/post! :automations {:name "flaw-unrecorded-run" :on :mechaniker
                                      :when [:= [:get :lizenz] "UR-1"]
                                      :effect {:action :mechaniker-umbenennen
                                               :args {:mechaniker {:self :id} :name "Umbenannt"}}
                                      :every-ms 3600000 :paused true})))
    (let [id      (automation-id "flaw-unrecorded-run")
          treffer (atom 0)]
      (is (some? id))
      (with-redefs [autos/record-run! (fn [& _]
                                        (swap! treffer inc)
                                        (throw (ex-info "injected before the run is recorded" {})))]
        (api/post! :automation-run {:id id} nil))
      (is (pos? @treffer) "der Fehler wurde nach der Wirkung eingeschleust")
      (let [gewirkt?     (= "Umbenannt" (some #(when (= "UR-1" (get-in % [:values :lizenz]))
                                                 (get-in % [:values :name]))
                                              (lesen :mechaniker)))
            protokoll?   (boolean (seq (laeufe id)))]
        (is (= gewirkt? protokoll?)
            (str "Wirkung " gewirkt? ", Run-Protokoll " protokoll?))))))

(deftest repeated-apply
  (testing "ein geplanter Lauf wendet seine Action nicht erneut auf dieselbe Objektversion an —
            auch nicht auf die, die seine eigene Wirkung hinterließ"
    (is (ok? (tun! :mechaniker-anlegen {:lizenz "RA-1" :name "Rosa" :muster "A320"})))
    (is (ok? (api/post! :automations {:name "flaw-repeated-apply" :on :mechaniker
                                      :when [:= [:get :lizenz] "RA-1"]
                                      :effect {:action :mechaniker-umbenennen
                                               :args {:mechaniker {:self :id} :name "Wiederholt"}}
                                      :every-ms 3600000 :paused true})))
    (let [id   (automation-id "flaw-repeated-apply")
          store   (ctx/store "main")
          branch   "main"
          auto (autos/get-one store branch id)]
      (is (= 1 (:applied (rules/run-one! store branch auto {}))) "der erste Lauf wirkt")
      (is (= 0 (:applied (rules/run-one! store branch auto {}))) "der zweite findet nichts Neues")
      (is (ok? (tun! :mechaniker-umbenennen
                     {:mechaniker (some #(when (= "RA-1" (get-in % [:values :lizenz])) (:id %))
                                        (lesen :mechaniker))
                      :name "von Hand"})))
      (is (= 1 (:applied (rules/run-one! store branch auto {})))
          "eine neue Version trifft wieder"))))
