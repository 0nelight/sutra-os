(ns ^:integration platform.flaw-c2-test
  (:require [support.system :as system]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.builds.import.changelog :as changelog]
            [sutra.builds.import.reconcile :as reconcile]
            [sutra.canvas.graphs :as cg]
            [sutra.store.control :as ctl]
            [sutra.store.access.run-log :as run-log]
            [sutra.store.access.scope :as ctx]
            [sutra.store.api :as api]
            [sutra.catalog.backing :as backing]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.world :as world]
            [sutra.actions.batch :as batch]
            [sutra.actions.dispatch :as c]
            [sutra.catalog.changes :as changes]
            [sutra.governance.branches :as onto-branches]
            [sutra.authoring.install.world :as onto-install]
            [support.in-project :refer [in-project]]
            [support.install :as install]
            [sutra.actions.publish :as onto-publish]
            [sutra.actions.proposals :as proposals]
            [sutra.actions.action :as action]
            [sutra.scenarios.apply :as scenario]
            [sutra.scenarios.state :as scenario-state]
            [sutra.store.seat :as seat]
            [support.pod :as pod]
            [support.store :as store])
  (:import (java.util.concurrent CyclicBarrier TimeUnit)))

(use-fixtures :once pod/with-mem-stores-only)

(defn- uid [] (subs (str (random-uuid)) 0 8))

(def ^:private WORLD
  (@#'onto-install/normalize-schema
   {:object-types {:widget {:object-type-name :widget
                            :attribute-list [{:attribute-name :name :attribute-type :string}]
                            :plural "Widgets"}}
    :link-types {} :interfaces {} :transforms {} :actions {} :object-sets {} :value-types {}}))

(def ^:private W (lookup/rid-of WORLD :object-types :widget))

(defn- fresh-world-branch! [prefix]
  (let [b (store/fresh-branch! prefix)]
    (store/install! b WORLD)
    b))

(defn- changes [b T id]
  (world/with-snapshot (ctx/system-store b)
    (fn [s] (api/read s api/unrestricted [:objects/changes T {:ids [(str id)]}]))))

(deftest unlogged-change
  (testing "ein Reindex, der ein Objekt entfernt, hinterlässt den Eintrag im Log (Änderungsfeed
            des Stores, :origin :index) — nicht nur eine verschwundene Zeile"
    (let [b  (fresh-world-branch! "flaw-reidx")
          id (str (random-uuid))]
      (try
        (world/index! (ctx/system-store b)
                      [(store/create-object W {:id id :values {:name "aus der Quelle"}})])
        (is (some? (store/object b W id)) "Voraussetzung: das Objekt steht im Index")
        (with-redefs [backing/object-backing        (fn [_ _] {:snapshot? true :dataset "flaw-ds"})
                      changelog/object-removals  (fn [& _] {:removed [id] :via :test})]
          (is (= [{:target W :via :test :removed 1}]
                 (reconcile/reconcile-object-tables! (ctx/system-store b) b WORLD {W []} nil nil))))
        (is (nil? (store/object b W id)) "der Reindex hat das Objekt entfernt")
        (let [del (filter #(= :delete-object (:op %)) (changes b W id))]
          (is (= 1 (count del)) "die Löschung steht im Änderungsfeed")
          (is (= :index (:origin (first del))) "mit ihrer Herkunft")
          (is (some? (:actor (first del))) "und wer sie schrieb"))
        (finally (store/retire! b))))))

(deftest partial-publish
  (testing "Fehler nach dem ersten Chunk: nichts veröffentlicht, oder der veröffentlichte Teil
            ist protokolliert (Marke/Logeintrag), sodass eine Fortsetzung ihn kennt"
    (let [b    (fresh-world-branch! "flaw-pub")
          h0   {:branch (seat/seated-branch)}
          n    (atom 0)
          real world/index!
          decls (mapv (fn [i] (changes/create-object (str (random-uuid)) W {:name (str "w" i)}))
                      (range 3))]
      (try
        (system/seat! (:branch h0))
        (with-redefs [batch/eval-chunk-rows 1
                      world/index! (fn [& args]
                                     (if (= 2 (swap! n inc))
                                       (throw
                                        (ex-info "eingeschleuster Fehler nach dem ersten Chunk" {}))
                                       (apply real args)))]
          (is (thrown? Exception
                       (onto-publish/publish! (ctx/store b grants/system-authority) WORLD decls
                                              (changes/publish-group (first decls)) nil))))
        (let [visible (count (store/objects b W))
              logged  (some #(<= visible (or (get-in % [:payload :published]) -1))
                            (run-log/action-log (ctx/system-store b)))]
          (is (or (zero? visible) logged)
              (str visible " Objekt(e) veröffentlicht, ohne Logeintrag/Marke für den Teil")))
        (finally
          (system/seat! (:branch h0))
          (store/retire! b))))))

(deftest two-defaults
  (testing "(Graph, default) ist eindeutig: zwei gleichzeitige Default-Setzungen ergeben ein Default"
    (let [store      (ctx/system-store)
          g       (str "flaw-graph-" (uid))
          barrier (CyclicBarrier. 2)
          real    ctl/list
          wait!   (fn [t where]
                    (when (and (= :color-schemes t) (:is-default where))
                      (try (.await barrier 3 TimeUnit/SECONDS) (catch Exception _ nil))))]
      (try
        (with-redefs [ctl/list (fn
                                  ([store t where] (wait! t where) (real store t where))
                                  ([store t where opts] (wait! t where) (real store t where opts)))]
          (let [fs (mapv (fn [nm]
                           (future (cg/upsert-color-scheme! store {:graph-id g :name nm :rules []
                                                                :is-default true})))
                         ["a" "b"])]
            (run! #(try @% (catch Exception _ nil)) fs)))
        (is (= 1 (count (filter :is-default (cg/query-color-schemes store g))))
            "genau ein Default-Farbschema je Graph")
        (finally
          (doseq [s (cg/query-color-schemes store g)] (cg/delete-color-scheme! store (:id s))))))))

(def ^:private ONTO
  {:objects {:ding {:name "Ding" :title :name :primary-key :name
                    :attributes [[:name :string] [:am :string] [:status :string]]
                    :plural "Dinge"}}
   :actions {:ding-anlegen   {:params {:name {:name "Name" :type :string :required true}
                                       :am   {:name "Am" :type :string :prefill :now}}
                              :rules  [{:op :create :object-type :ding
                                        :values {:name [:get :name] :am [:get :am]}}]
                              :edits  [{:object-type :ding}]}
             :ding-markieren {:params {:ding {:name "Ding" :type :ding :required true}}
                              :rules  [{:op :update :object-type :ding :target [:get :ding]
                                        :values {:status "markiert"}}]
                              :edits  [{:object-type :ding}]}}})

(defn- with-scenario
  [f]
  (let [t  (store/fresh-branch! "flaw-sc-target")
        sc (str "scenario/flaw-c2/" (uid))]
    (try
      (let [w (:ok (onto-install/prepare-schema (in-project :c2 ONTO)
                                                {:old-schema
                                                 (delay (world/authored (ctx/system-store t)))}))]
        (install/seat-on-main! w)
        (onto-install/install-on-branch! (ctx/system-store t) w))
      (scenario/create-scenario-branch! (ctx/system-store) {:parent t :name sc})
      (f t sc (lookup/rid-of (seat/branch-schema (ctx/system-store t)) :object-types :ding))
      (finally
        (try (onto-branches/drop-branch! (ctx/system-store) sc) (catch Throwable _ nil))
        (try (store/retire! t) (catch Throwable _ nil))))))

(defn- dispatch! [sc action args]
  (c/dispatch-action! (ctx/at (ctx/store "main" grants/system-authority) sc)
                      (seat/branch-schema (ctx/system-store sc)) action args))

(defn- dinge [b T]
  (into {} (map (fn [o] [(str (:id o)) (:values o)])) (store/objects b T)))

(deftest replay-drift
  (testing "Szenario anwenden: now und fresh-id kommen aus dem Oracle-Protokoll des Szenarios (A4) —
            auf dem Ziel entsteht dasselbe Objekt wie im Szenario"
    (with-scenario
      (fn [t sc T]
        (dispatch! sc :ding-anlegen {:name "eins"})
        (let [im-szenario (dinge sc T)]
          (is (= 1 (count im-szenario)) "Voraussetzung: das Szenario hat ein Ding")
          (Thread/sleep 20)
          (scenario/apply-scenario! (ctx/store "main" grants/system-authority) sc t)
          (let [auf-ziel (dinge t T)]
            (is (= (set (keys im-szenario)) (set (keys auf-ziel)))
                "dieselbe ID (fresh-id aus dem Protokoll)")
            (is (= (map :am (vals im-szenario)) (map :am (vals auf-ziel)))
                "dieselbe Zeit (now aus dem Protokoll)")))))))

(deftest deferred-id-drift
  (testing "Szenario mit Create ohne eigene ID und Folge-Action darauf anwenden: die Folge-Action
            trifft das Objekt auf dem Ziel (Stellvertreter für den aufgeschobenen Writeback-Create,
            dessen Identität beim Replay ebenso neu entsteht)"
    (with-scenario
      (fn [t sc T]
        (dispatch! sc :ding-anlegen {:name "zwei"})
        (let [id (ffirst (dinge sc T))]
          (dispatch! sc :ding-markieren {:ding id})
          (is (= "markiert" (:status (get (dinge sc T) id))) "Voraussetzung: im Szenario getroffen")
          (let [r (try (scenario/apply-scenario! (ctx/store "main" grants/system-authority) sc t)
                       (catch clojure.lang.ExceptionInfo e {:error (ex-message e)}))]
            (is (nil? (:error r)) (str "das Szenario wendet sich an: " (:error r)))
            (is (= ["markiert"] (mapv :status (vals (dinge t T))))
                "die Folge-Action hat das angelegte Ding getroffen")))))))

(deftest sealed-append
  (testing "eine Action, die den Guard passiert hat, landet nicht mehr, wenn das Szenario
            vor ihrem Commit versiegelt wurde"
    (with-scenario
      (fn [_ sc T]
        (let [orig scenario-state/append-fence]
          (with-redefs [scenario-state/append-fence (fn [store b]
                                                      (let [f (orig store b)]
                                                        (scenario-state/seal! store b)
                                                        f))]
            (is (thrown? Exception (dispatch! sc :ding-anlegen {:name "zu spät"})))))
        (is (empty? (dinge sc T)) "das versiegelte Szenario hat nichts angenommen")))))

(deftest partial-apply
  (testing "scheitert eine Action beim Anwenden, landet keine"
    (with-scenario
      (fn [t sc T]
        (dispatch! sc :ding-anlegen {:name "eins"})
        (dispatch! sc :ding-anlegen {:name "zwei"})
        (let [n    (atom 0)
              orig action/run-held]
          (with-redefs [action/run-held (fn [req action args extra]
                                          (when (= 2 (swap! n inc))
                                            (throw (ex-info "eingeschleust: zweite Action" {})))
                                          (orig req action args extra))]
            (is (thrown? Exception
                         (scenario/apply-scenario! (ctx/store "main" grants/system-authority) sc
                                                   t)))))
        (is (empty? (dinge t T)) "nichts angewandt")
        (is (not (scenario/scenario-applied? (ctx/system-store sc))))))))

(deftest double-apply
  (testing "zwei gleichzeitige Anwendungen: eine landet, die andere nicht"
    (with-scenario
      (fn [t sc T]
        (dispatch! sc :ding-anlegen {:name "einmal"})
        (let [bar (CyclicBarrier. 2)
              rs  (mapv deref
                        (vec (repeatedly 2 #(future (.await bar 5 TimeUnit/SECONDS)
                                               (try (scenario/apply-scenario!
                                                     (ctx/store "main" grants/system-authority) sc
                                                     t)
                                                    (catch Exception e
                                                      {:error (ex-message e)}))))))]
          (is (= 1 (count (remove :error rs))) (pr-str rs))
          (is (= 1 (count (dinge t T))) "genau einmal angewandt"))))))

(defn- namen [b T] (set (map :name (vals (dinge b T)))))

(deftest lost-scenario-action
  (testing "eine Action, die während des Rebuilds an das Szenario angehängt wird, geht nicht verloren"
    (with-scenario
      (fn [t sc T]
        (dispatch! sc :ding-anlegen {:name "eins"})
        (let [orig  @#'scenario/build-aside!
              erst? (atom true)]
          (with-redefs [scenario/build-aside! (fn [& args]
                                                (let [r (apply orig args)]
                                                  (when (compare-and-set! erst? true false)
                                                    (dispatch! sc :ding-anlegen {:name "zwei"}))
                                                  r))]
            (scenario/rebuild-scenario! (ctx/store "main" grants/system-authority) sc t)))
        (is (= #{"eins" "zwei"} (namen sc T)) "der Rebuild hat die nachgereichte Action übernommen")
        (is (= 2
               (count
                (:replayable
                 (scenario/scenario-definition (ctx/system-store sc)
                                               (seat/branch-schema (ctx/system-store sc)))))))))))

(deftest partial-rebuild
  (testing "bricht der Rebuild vor dem Umschalten ab, bleibt das Szenario, wie es war"
    (with-scenario
      (fn [t sc T]
        (dispatch! sc :ding-anlegen {:name "eins"})
        (with-redefs [world/adopt! (fn [& _] (throw (ex-info "eingeschleust: Absturz" {})))]
          (is (thrown? Exception
                       (scenario/rebuild-scenario! (ctx/store "main" grants/system-authority) sc
                                                   t))))
        (is (world/exists? (ctx/system-store) sc))
        (is (= #{"eins"} (namen sc T)))
        (is (= 1 (count (run-log/action-log (ctx/system-store sc)))) "mit seinem Action-Log")))))

(deftest partial-duplicate
  (testing "bricht duplicate vor dem Umschalten ab, gibt es keine halbe Kopie"
    (with-scenario
      (fn [t sc T]
        (dispatch! sc :ding-anlegen {:name "eins"})
        (dispatch! sc :ding-anlegen {:name "zwei"})
        (let [dst (str "scenario/flaw-c2/" (uid))]
          (try
            (let [n    (atom 0)
                  orig action/run-action!]
              (with-redefs [action/run-action! (fn [req]
                                                 (when (= 2 (swap! n inc))
                                                   (throw
                                                    (ex-info
                                                     "eingeschleust: Absturz nach der ersten Kopie"
                                                     {})))
                                                 (orig req))]
                (is (thrown? Exception
                             (scenario/duplicate-scenario!
                              (ctx/store "main" grants/system-authority)
                              sc t dst identity {})))))
            (is (not (world/exists? (ctx/system-store) dst)) "keine halbe Kopie")
            (let [r (scenario/duplicate-scenario! (ctx/store "main" grants/system-authority) sc t
                                                  dst
                                                  identity {})]
              (is (= 2 (:replayed r)))
              (is (= #{"eins" "zwei"} (namen dst T)))
              (is (= 2 (count (run-log/action-log (ctx/system-store dst))))
                  "die Kopie hat ihr Action-Log"))
            (finally (try (onto-branches/drop-branch! (ctx/system-store) dst)
                       (catch Throwable _ nil)))))))))

(defn- vorschlag! [t id]
  (proposals/propose! (ctx/store t grants/system-authority) t
                      {:automation-id "flaw-d138" :object-id id :action :ding-markieren
                       :args {:ding id} :reason "test"}))

(defn- vorschlag [t id]
  (some #(when (= id (:object-id %)) %)
        (proposals/list-for (ctx/store t grants/system-authority) t "flaw-d138" nil)))

(deftest stale-proposal-und-applied-not-run
  (with-scenario
    (fn [t _ T]
      (let [sys    (ctx/store t grants/system-authority)
            schema (seat/branch-schema (ctx/system-store t))
            neu!   (fn [nm] (c/dispatch-action! sys schema :ding-anlegen {:name nm})
                     (some (fn [[id v]] (when (= nm (:name v)) id)) (dinge t T)))]
        (testing "stale-proposal: das Objekt hat sich seit dem Vorschlag geändert"
          (let [id (neu! "alt")]
            (vorschlag! t id)
            (c/dispatch-action! sys schema :ding-markieren {:ding id})
            (let [r (proposals/accept! sys t schema "flaw-d138" (:id (vorschlag t id)))]
              (is (= "failed" (:status r)) (pr-str r))
              (is (= "failed" (:status (vorschlag t id)))))))
        (testing "applied-not-run: scheitert die Action, steht der Vorschlag nie auf applied"
          (let [id (neu! "nie")]
            (vorschlag! t id)
            (with-redefs [action/finish! (fn [& _] (throw (ex-info "eingeschleust" {})))]
              (proposals/accept! sys t schema "flaw-d138" (:id (vorschlag t id))))
            (is (not= "applied" (:status (vorschlag t id))))
            (is (nil? (:status (get (dinge t T) id))) "die Action lief nicht")))
        (testing "unverändert angenommen: Wirkung und applied zusammen"
          (let [id (neu! "gut")]
            (vorschlag! t id)
            (is (= "applied" (:status (proposals/accept! sys t schema "flaw-d138"
                                                         (:id (vorschlag t id))))))
            (is (= "markiert" (:status (get (dinge t T) id))))))))))
