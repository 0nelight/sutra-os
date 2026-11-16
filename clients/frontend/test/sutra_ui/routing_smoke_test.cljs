(ns sutra-ui.routing-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.state.portal :as pstate]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.routing :as routing]
            [sutra-ui.feature :as feature]
            [sutra-ui.apps.all]
            [sutra-ui.apps.launcher.feature :as launcher]))

(def ^:private rechnung-rid
  "ri.sutra.main.object-type.1a373f7c-37ca-4d31-a9c1-6bf1fa1aef71")
(def ^:private partner-rid
  "ri.sutra.main.object-type.2b484a8d-48db-4e42-b2d2-7c1c2bf2bf82")

(def ^:private instance-id "01b8556a-fcdf-561e-8400-2bdb411cff4c")

(defn- with-schema-index!
  []
  (reset! catalog/rid->name {rechnung-rid :rechnung
                             partner-rid  :geschäftspartner})
  (reset! catalog/name->rid {:object-types {:rechnung        rechnung-rid
                                            :geschäftspartner partner-rid}}))

(defn- no-schema-index! []
  (reset! catalog/rid->name {})
  (reset! catalog/name->rid {}))

(defn- object-state [type-kw]
  (feature/arrive {:current-branch "main"} :object-view
                  {:object-type type-kw :instance-id instance-id}))

(defn- reminted
  [path]
  (routing/state->path (routing/route->ui {} (routing/parse-path path))))

(deftest object-view-url-names-its-type
  (with-schema-index!)
  (testing "the type segment is the readable name, not a second opaque id"
    (is (= (str "/object/rechnung/" instance-id)
           (routing/state->path (object-state :rechnung)))))
  (testing "a non-ASCII type name is percent-encoded, not dropped or mangled"
    (is (= (str "/object/gesch%C3%A4ftspartner/" instance-id)
           (routing/state->path (object-state :geschäftspartner))))))

(deftest the-default-branch-is-never-written
  (testing "`main` is the default, so it takes no segment — the URL people
            paste is /object-workspace, not /branch/main/object-workspace"
    (is (= "/object-workspace"
           (routing/state->path {:current-branch "main" :nav :object-workspace}))))
  (testing "any OTHER branch still names itself, or the path could not say
            which ontology the page is reading"
    (is (= "/branch/feature-x/object-workspace"
           (routing/state->path {:current-branch "feature-x"
                                 :nav :object-workspace}))))
  (testing "a saved /branch/main/… link keeps parsing — read the retired
            shape, mint only the new one (same trade as legacy-nav->str)"
    (is (= "main" (:current-branch (routing/parse-path "/branch/main/object-workspace"))))
    (is (= :object-workspace (:nav (routing/parse-path "/branch/main/object-workspace")))))
  (testing "and re-minting from it drops the redundant head"
    (is (= "/object-workspace" (reminted "/branch/main/object-workspace")))))

(deftest object-view-round-trips
  (with-schema-index!)
  (doseq [t [:rechnung :geschäftspartner]]
    (testing (str "encode → parse → same selection for " t)
      (let [route (routing/parse-path (routing/state->path (object-state t)))]
        (is (= :object-view (:nav route)))
        (is (nil? (:current-branch route))
            "the default branch left no segment, so nothing parses back —
             apply-route! substitutes it")
        (is (= {:object-type t :instance-id instance-id}
               (:params route))
            "the parsed selection is the name-kw nav-state holds, so re-minting
             the path from it is a fixed point")))))

(deftest minted-path-is-a-fixed-point
  (with-schema-index!)
  (testing "re-encoding a parsed route yields the identical path (no drift)"
    (let [p1 (routing/state->path (object-state :rechnung))
          p2 (reminted p1)]
      (is (= p1 p2)))))

(deftest legacy-rid-urls-still-resolve
  (with-schema-index!)
  (let [legacy (str "/branch/main/object/" rechnung-rid "/" instance-id)]
    (testing "a saved /object/<rid>/<id> link parses to the same state as the
              new /object/<name>/<id> one"
      (is (= (:params (routing/parse-path legacy))
             {:object-type :rechnung :instance-id instance-id})))
    (testing "and re-minting from that state upgrades the URL in place —
              both halves at once: the RID becomes the name, the default
              branch loses its head"
      (is (= (str "/object/rechnung/" instance-id)
             (reminted legacy))))))

(deftest legacy-rid-survives-a-cold-parse
  (no-schema-index!)
  (let [legacy (str "/object/" rechnung-rid "/" instance-id)
        route  (routing/parse-path legacy)]
    (testing "before the schema snapshot lands the RID is kept UNRESOLVED —
              never nil, or the page would flash 'object not found'"
      (is (= rechnung-rid (get-in route [:params :object-type]))))
    (testing "and encoding it back does not invent a name"
      (is (= legacy (reminted legacy))))))

(deftest a-dead-type-name-parses-to-something-nameable
  (with-schema-index!)
  (testing "a link to a since-renamed type keeps its name-kw rather than
            collapsing to nil — the Object View can then say WHICH type it
            could not find instead of rendering an empty page"
    (let [route (routing/parse-path (str "/object/rechnung-alt/" instance-id))]
      (is (= {:object-type :rechnung-alt :instance-id instance-id}
             (:params route))))))

(deftest admin-routes-still-mint-rids
  (with-schema-index!)
  (testing "only the Object View trades rename-stability for readability;
            ontology detail keeps the RID"
    (is (= (str "/ontology/object-types/" rechnung-rid)
           (routing/state->path
            (feature/arrive {} :ontology {:section :object-types
                                          :detail  {:kind :object-type
                                                    :id   :rechnung
                                                    :tab  :overview}}))))
    (is (= :object-workspace (:nav (routing/parse-path (str "/entities/" rechnung-rid)))))))

(deftest partial-object-selection-emits-no-tail
  (with-schema-index!)
  (testing "half a selection must not mint /object/<type> — that path parses
            back to no selection and would push a dead history entry"
    (is (= "/object"
           (routing/state->path (feature/arrive {} :object-view {:object-type :rechnung}))))
    (is (nil? (get-in (routing/parse-path "/object") [:params :object-type])))))

(def ^:private gf
  {:id "ri.sutra.main.workspace.9c0d1e2f-3a4b-4c5d-8e9f-0a1b2c3d4e5f"
   :name "Geschäftsführung"
   :apps [{:id "ri.app.1" :name :leitwarte}]})

(defn- inside! [ws open]
  (reset! auth/current-workspace ws)
  (reset! pstate/current-consumer-app open))

(defn- outside! [] (inside! nil nil))

(deftest workspace-owns-the-whole-path
  (outside!)
  (testing "the workspace's name is what the link carries, percent-encoded"
    (is (= "/workspace/Gesch%C3%A4ftsf%C3%BChrung"
           (routing/workspace-path gf nil))))
  (testing "an open module hangs off it, so a link can name the screen"
    (is (= "/workspace/Gesch%C3%A4ftsf%C3%BChrung/app/leitwarte"
           (routing/workspace-path gf {:card {:name :leitwarte}})))))

(deftest inside-a-workspace-the-shell-path-steps-aside
  (testing "the ui-state watcher must not keep writing /apps over a surface
            that is no longer showing"
    (inside! gf nil)
    (is (= "/workspace/Gesch%C3%A4ftsf%C3%BChrung"
           (routing/state->path {:nav :applications})))
    (outside!)
    (is (= "/apps" (routing/state->path {:nav :applications})))))

(deftest workspace-paths-round-trip
  (testing "what it mints parses back to the state it was minted from"
    (is (= {:workspace-slug "Geschäftsführung" :open-app-slug nil}
           (routing/parse-path (routing/workspace-path gf nil))))
    (is (= {:workspace-slug "Geschäftsführung" :open-app-slug "leitwarte"}
           (routing/parse-path
            (routing/workspace-path gf {:card {:name :leitwarte}}))))))

(deftest a-workspace-route-carries-no-shell-state
  (testing "no branch, no nav, no detail — the surface it names has none"
    (let [r (routing/parse-path "/workspace/Geschäftsführung")]
      (is (nil? (:nav r)))
      (is (nil? (:current-branch r))))))

(deftest a-workspace-resolves-by-name-or-by-id
  (let [wss [gf]]
    (testing "a link typed by hand differs in case from one that was copied"
      (is (= gf (routing/workspace-of-slug "geschäftsführung" wss))))
    (testing "the title the portal shows resolves too, whatever the name is"
      (is (= (assoc gf :name "gebaeude-und-technik" :display-name "Gebäude & Technik")
             (routing/workspace-of-slug
              "Gebäude & Technik"
              [(assoc gf :name "gebaeude-und-technik" :display-name "Gebäude & Technik")]))))
    (testing "the id shape still resolves, the way legacy nav segments do"
      (is (= gf (routing/workspace-of-slug (:id gf) wss))))
    (testing "a workspace that is gone (or was never granted) resolves to
              nothing, so the caller can leave the user in the portal"
      (is (nil? (routing/workspace-of-slug "hangar" wss)))
      (is (nil? (routing/workspace-of-slug "" wss))))))

(deftest workspace-frame-edit-ist-keine-arbeitsplatz-adresse
  (testing "der Editor-Pfad wird als Editor erkannt, auch mit Schrägstrich"
    (is (routing/editor-path? "/workspace/workspace-frame/edit"))
    (is (routing/editor-path? "/workspace/workspace-frame/edit/")))
  (testing "und nicht als Slug gelesen"
    (is (nil? (routing/parse-workspace-path "/workspace/workspace-frame/edit"))))
  (testing "ein echter Arbeitsplatz wird weiterhin gelesen"
    (is (= {:workspace-slug "geschaeftsfuehrung" :open-app-slug nil}
           (routing/parse-workspace-path "/workspace/geschaeftsfuehrung")))
    (is (= {:workspace-slug "geschaeftsfuehrung" :open-app-slug "leitwarte"}
           (routing/parse-workspace-path "/workspace/geschaeftsfuehrung/app/leitwarte"))))
  (testing "ein Arbeitsplatz DARF workspace-frame heißen, solange er nicht /edit ist"
    (is (= {:workspace-slug "workspace-frame" :open-app-slug nil}
           (routing/parse-workspace-path "/workspace/workspace-frame")))))

(deftest one-app-link-for-every-role
  (testing "the portal's /app/<name> opens the app in the builder shell too —
            it used to fall through to the default page without a word"
    (let [r (routing/parse-path "/app/leitwarte")]
      (is (= :applications (:nav r)))
      (is (= "leitwarte" (get-in r [:params :open-app-slug]))))
    (is (= (select-keys (routing/parse-path "/apps/leitwarte") [:nav :params])
           (select-keys (routing/parse-path "/app/leitwarte") [:nav :params]))))
  (testing "and the consumer portal reads the builder's /apps/<name>"
    (let [parse #(#'routing/match :consumer % {})]
      (is (= [:portal {:open-app-slug "leitwarte"}] (parse "/apps/leitwarte")))
      (is (= [:portal {:open-app-slug "leitwarte"}] (parse "/app/leitwarte")))
      (is (= [:portal {:open-app-slug "leitwarte" :version "dev"}]
             (parse "/apps/leitwarte/dev")))))
  (testing "one table: the editor, a workspace and the bare portal are routes of the
            consumer's mode, the builder never reads the portal or the editor"
    (is (= [:editor {:editor? true}]
           (#'routing/match :consumer "/workspace/workspace-frame/edit" {})))
    (is (= :workspace (first (#'routing/match :consumer "/workspace/leitwarte" {}))))
    (is (= [:portal {}] (#'routing/match :consumer "/" {})))
    (is (= :shell (first (#'routing/match :builder "/workspace/workspace-frame/edit" {}))))
    (is (= :shell (first (#'routing/match :builder "/apps/leitwarte" {})))))
  (testing "both shells mint the one shape"
    (outside!)
    (is (= "/apps/leitwarte"
           (#'routing/consumer-state->path {:card {:name :leitwarte}})))))

(deftest every-registered-screen-round-trips-its-address
  (is (seq (feature/screens)) "the apps registered themselves")
  (doseq [{:keys [id]} (feature/screens)]
    (is (= id (:nav (routing/parse-path (routing/state->path {:nav id}))))
        (str id " opens again from the address it writes"))))

(deftest every-screen-parses-a-bare-address-to-params-it-takes
  (testing "an address replaces what it owns, so `:parse-tail` of nothing names every
            param it would set — and only params the screen accepts"
    (doseq [{:keys [id params parse-tail]} (feature/screens) :when parse-tail]
      (is (every? (set params) (keys (parse-tail []))) (str id)))))

(deftest arriving-is-pure-and-checks-the-contract
  (testing "params land in the slice of the screen's app, nowhere else"
    (let [ui (feature/arrive {:nav :kompass} :notebook {:doc "d1"})]
      (is (= :notebook (:nav ui)))
      (is (= {:doc "d1"} (feature/slice ui :notebook)))
      (is (= #{:nav :slices} (set (keys ui))))))
  (testing "a param the screen does not take is a broken link, not a silent no-op"
    (is (thrown? js/Error (feature/arrive {} :notebook {:notebook-doc "d1"}))))
  (testing "an address decides the screen it names and leaves every other app's slice:
            whoever comes back to the explorer finds the type they left it on"
    (let [ui (-> {}
                 (feature/arrive :object-workspace {:object-type :rechnung})
                 (routing/route->ui (routing/parse-path "/notebook")))]
      (is (= :notebook (:nav ui)))
      (is (= :rechnung (:object-type (feature/slice ui :explorer))))
      (is (= "/object-workspace/rechnung"
             (routing/state->path (feature/arrive ui :object-workspace {}))))))
  (testing "a screen nobody registered leaves the state as it was"
    (is (= {:nav :kompass} (feature/arrive {:nav :kompass} :no-such-screen {:x 1})))))

(deftest the-root-address-names-home
  (testing "`/` is home: whoever comes back to it leaves the screen they were on"
    (let [ui (routing/route->ui {:nav :notebook} (routing/parse-path "/"))]
      (is (nil? (:nav ui)))
      (is (= (feature/home-nav) (feature/current-nav ui)))
      (is (= "/" (routing/state->path ui))))))

(deftest every-app-opens-on-a-screen-it-owns
  (doseq [{:keys [id]} (feature/apps)]
    (is (= id (feature/app-of (feature/app-default-nav id))) (str id))))

(defn- entered
  [path search]
  (let [route (routing/parse-path path search)]
    [route (routing/state->path (routing/route->ui {} route))]))

(deftest explorer-writes-type-perspective-and-set
  (with-schema-index!)
  (doseq [p ["/object-workspace/rechnung"
             "/object-workspace/rechnung/charts"
             "/object-workspace/rechnung/map"
             "/object-workspace/ri.sutra.main.temporary-object-set.9a1b"]]
    (testing (str p " is its own fixed point")
      (is (= p (reminted p))))))

(deftest explorer-reads-counterparts-ways-in
  (with-schema-index!)
  (testing "?objectTypeId opens the type; results is the table, else the charts"
    (let [[r p] (entered "/object-workspace" "?objectTypeId=rechnung&perspectiveId=results")]
      (is (:entry-form? r))
      (is (= "/object-workspace/rechnung" p)))
    (is (= "/object-workspace/rechnung/charts"
           (second (entered "/object-workspace" "?objectTypeId=rechnung")))))
  (testing "a saved exploration or object set by its RID"
    (is (= "/object-workspace/ri.sutra.main.object-set.77"
           (second (entered "/object-workspace/saved/ri.sutra.main.object-set.77" nil))))
    (is (= "/object-workspace/ri.sutra.main.temporary-object-set.9a"
           (second (entered "/object-workspace/objectSet/ri.sutra.main.temporary-object-set.9a"
                            nil)))))
  (testing "a keyword search lands on the Explorer's home with the text"
    (let [[r p] (entered "/object-workspace/keyword/hello%20world" nil)]
      (is (= "hello world" (get-in r [:params :keyword])))
      (is (= "/object-workspace" p))))
  (testing "a complex search becomes the type's filters"
    (let [json  (js/encodeURIComponent
                 (js/JSON.stringify
                  (clj->js {"keyword" "x" "objectTypes" ["rechnung"]
                            "filters" [{"type" "propertyFilter" "objectType" "rechnung"
                                        "propertyType" "betrag"
                                        "value" {"type" "numberRangeFilter" "min" 10}}
                                       {"type" "propertyFilter" "propertyType" "status"
                                        "value" {"type" "valuesFilter" "values" ["offen"]}}
                                       {"type" "linkFilter" "linkType" "l"}]})))
          [r p] (entered (str "/object-workspace/search/" json) nil)
          ui    (routing/route->ui {} r)
          set   (get-in (feature/slice ui :explorer) [:set :rechnung])]
      (is (= "/object-workspace/rechnung" p))
      (is (= "x" (:search set)))
      (is (= [[:betrag :gte "10"] [:status :eq "offen"]]
             (mapv (juxt :field :op :value) (:filters set)))))))

(deftest object-links-by-key-and-embedded
  (with-schema-index!)
  (testing "/object/v0/<type>?<key>=<value> looks the object up and writes nothing yet"
    (let [[r p] (entered "/object/v0/rechnung" "?nummer=4711")]
      (is (= [:nummer "4711"] (get-in r [:params :key])))
      (is (nil? p) "the address waits for the lookup instead of minting /object")))
  (testing "/objects/<type>/<id> is the Object View without the shell"
    (let [[r p] (entered (str "/objects/rechnung/" instance-id) nil)]
      (is (:embedded? r))
      (is (= (str "/object/rechnung/" instance-id) p)))))

(deftest open-in-offers-what-takes-the-value
  (let [ids (fn [v] (set (map :id (feature/open-in-targets v))))]
    (testing "an object opens in an Object View, and as a set of one in the Explorer"
      (is (= #{:object-view :object-finder}
             (ids {:kind :object :object-type :rechnung :id "1"}))))
    (testing "a set never in an Object View"
      (is (= #{:object-finder}
             (ids {:kind :object-set :object-type :rechnung :query {}}))))
    (testing "the set of one names the object"
      (is (= {:ids ["1"]}
             (:query (some #(when (= :object-finder (:id %)) (:value %))
                           (feature/open-in-targets
                            {:kind :object :object-type :rechnung :id "1"}))))))))

(deftest workspaces-make-apps-discoverable
  (let [leitwarte {:id "ri.app.l" :name :leitwarte :display-name "Leitwarte"
                   :inputs [{:external-id :auftrag :object-type :rechnung :set? false}
                            {:external-id :posten :object-type :rechnung :set? true}]}
        x         {:id "ri.app.x" :name :x
                   :inputs [{:external-id :a :object-type :rechnung :set? false}]}
        wss       [{:promoted true :discoverable [leitwarte]}
                   {:promoted false :discoverable [x]}]
        targets   (launcher/discoverable-targets wss nil)
        taking    (fn [v] (filter #((:accepts? %) v) targets))]
    (testing "outside a workspace: the promoted workspaces' modules, once per input"
      (is (= #{"ri.app.l"} (set (map (comp second :id) targets))))
      (is (= 2 (count targets))))
    (testing "inside one: only what that workspace offers"
      (is (= #{"ri.app.x"}
             (set (map (comp second :id) (launcher/discoverable-targets wss (second wss)))))))
    (testing "an object goes to the object input, as its id in the URL"
      (let [[t] (taking {:kind :object :object-type :rechnung :id "42"})]
        (is (= "/apps/leitwarte?auftrag=42" ((:href t) {:kind :object :id "42"})))))
    (testing "a set only as ids, and never empty"
      (is (= 1 (count (taking {:kind :object-set :object-type :rechnung
                               :query {:ids ["1" "2"]}}))))
      (is (empty? (taking {:kind :object-set :object-type :rechnung
                           :query {:where {:status {:= "offen"}}}})))
      (is (empty? (taking {:kind :object-set :object-type :rechnung :query {:ids []}}))))
    (testing "another type is not taken"
      (is (empty? (taking {:kind :object :object-type :partner :id "1"}))))))

(deftest a-document-opens-for-reading
  (testing "/notebook/<doc>/view is read-only and its own fixed point"
    (is (= "/notebook/d1/view" (reminted "/notebook/d1/view")))
    (is (true? (get-in (routing/parse-path "/notebook/d1/view") [:params :read-only?]))))
  (testing "the plain address is for writing"
    (is (= "/notebook/d1" (reminted "/notebook/d1")))
    (is (false? (get-in (routing/parse-path "/notebook/d1") [:params :read-only?])))))
