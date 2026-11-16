(ns sutra-ui.apphost.view-state-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [reagent.dom.server :as rds]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.apphost.sdk :as app]
            [sutra-ui.apphost.render :as render]
            [sutra-ui.apphost.sci-runtime :as sci-rt]
            [sutra-ui.apphost.view-state :as vs]))

(defn- spy
  []
  (let [log (atom [])]
    {:log log :fn (fn [m push?] (swap! log conj [m push?]))}))

(defn- handle [k default opts]
  (let [store (atom {})
        s     (spy)
        cur   (vs/make store k default opts (:fn s))]
    {:store store :log (:log s) :cur cur}))

(deftest edn-default-roundtrips-any-value
  (testing ":edn (default) carries arbitrary values with full fidelity"
    (let [{:keys [cur store]} (handle :sel nil nil)]
      (is (nil? @cur) "URL omits the key → default")
      (reset! cur #{:a :b})
      (is (= #{:a :b} @cur) "a set round-trips through the URL string")
      (is (string? (get @store "sel")) "the store holds the raw encoded string"))))

(deftest ein-blosses-wort-in-der-url-bleibt-text
  (testing "ein bares Wort wird zu TEXT, nicht zu einem Symbol"
    (let [{:keys [cur store]} (handle :plan-id nil nil)]
      (swap! store assoc "plan-id" "hangar-1")
      (is (= "hangar-1" @cur))
      (is (string? @cur) "ein Symbol waere gegen jeden Zeilenwert ungleich")))
  (testing "auch mit Leerzeichen — read-string las sonst nur die ERSTE Form"
    (let [{:keys [cur store]} (handle :q nil nil)]
      (swap! store assoc "q" "Hangar 1")
      (is (= "Hangar 1" @cur) "der Rest darf nicht stillschweigend wegfallen")))
  (testing "was EDN wirklich meint, bleibt EDN"
    (let [{:keys [cur store]} (handle :sel nil nil)]
      (doseq [[roh erwartet] [["42" 42] [":orders" :orders] ["true" true]
                              ["nil" nil] ["[1 2]" [1 2]] ["\"schon Text\"" "schon Text"]]]
        (swap! store assoc "sel" roh)
        (is (= erwartet @cur) (str roh " muss " (pr-str erwartet) " bleiben")))))
  (testing "die eigene Schreibweise der App geht unveraendert hin und zurueck"
    (let [{:keys [cur]} (handle :plan-id nil nil)]
      (reset! cur "hangar-1")
      (is (= "hangar-1" @cur)))))

(deftest presets-give-human-readable-text
  (testing ":string stores the bare value"
    (let [{:keys [cur store]} (handle :q "" {:as :string})]
      (reset! cur "müller")
      (is (= "müller" @cur))
      (is (= "müller" (get @store "q")) "no EDN quoting in the URL")))
  (testing ":keyword ⇄ name"
    (let [{:keys [cur store]} (handle :tab :overview {:as :keyword})]
      (reset! cur :orders)
      (is (= :orders @cur))
      (is (= "orders" (get @store "tab")))))
  (testing ":keyword schluckt einen führenden Doppelpunkt beim LESEN"
    (let [{:keys [cur store]} (handle :tab :overview {:as :keyword})]
      (swap! store assoc "tab" ":orders")
      (is (= :orders @cur))
      (swap! store assoc "tab" "orders")
      (is (= :orders @cur) "und die neue Schreibweise natürlich auch")))
  (testing ":keyword — ein LEERER Parameter ist der Default, nicht das leere Keyword"
    (let [{:keys [cur store]} (handle :tab :overview {:as :keyword})]
      (swap! store assoc "tab" "")
      (is (= :overview @cur))
      (swap! store assoc "tab" ":")
      (is (= :overview @cur) "auch ein blosser Doppelpunkt sagt nichts")))
  (testing ":int parses, NaN falls back to default"
    (let [{:keys [cur store]} (handle :id 0 {:as :int})]
      (reset! cur 42)
      (is (= 42 @cur))
      (is (= "42" (get @store "id")))
      (swap! store assoc "id" "not-a-number")
      (is (= 0 @cur) "an unparseable URL value yields the default, not a crash"))))

(deftest default-value-stays-out-of-the-url
  (testing "writing the default DROPS the param, so the URL is minimal + stable"
    (let [{:keys [cur store]} (handle :tab :overview {:as :keyword})]
      (reset! cur :orders)
      (is (contains? @store "tab"))
      (reset! cur :overview)
      (is (not (contains? @store "tab"))
          "a value equal to the default must not linger as a URL param"))))

(deftest bool-absence-is-false
  (testing ":bool encodes only the true case"
    (let [{:keys [cur store]} (handle :open? false {:as :bool})]
      (reset! cur true)
      (is (true? @cur))
      (is (= "1" (get @store "open?")))
      (reset! cur false)
      (is (false? @cur))
      (is (not (contains? @store "open?"))))))

(deftest swap-and-push-intent
  (testing "swap! reads-then-writes; commit carries the :push? intent"
    (let [{:keys [cur log]} (handle :n 0 {:as :int :push? true})]
      (swap! cur inc)
      (swap! cur inc)
      (is (= 2 @cur))
      (is (every? (fn [[_ push?]] (true? push?)) @log)
          "every commit under :push? true asks for a new history entry"))
    (testing "default is replace-in-place"
      (let [{:keys [cur log]} (handle :q "" {:as :string})]
        (reset! cur "a")
        (is (every? (fn [[_ push?]] (false? push?)) @log))))))

(deftest view-uses-it-through-the-sci-seam
  (testing "a view calls app/view-state, resolves through the SCI ctx, and renders
            SSR-safe (install! must not touch an absent js/window in node)"
    (let [src  (str "(defn app [_ _]\n"
                    "  (let [tab (app/view-state :tab :overview {:as :keyword})]\n"
                    "    (fn [_ _] [:div {:class \"capp\"} \"tab=\" (name @tab)])))\n"
                    "app\n")
          comp (sci-rt/compile-view src)]
      (is (fn? comp) "app/view-state resolved in the SCI ctx (surface is exported)")
      (let [html (rds/render-to-string [comp {} {}])]
        (is (not (re-find #"konnte nicht geladen" html))
            "constructing a view-state during render must not hit the SCI error path")
        (is (re-find #"tab=(<!-- -->)?overview" html)
            "with no URL param the handle derefs to its default")))))

(defn- ctrl [] (vs/make (atom {}) :ov {} {:as :edn} (fn [_ _] nil)))

(deftest overlay-controller-semantics
  (let [ov (ctrl)]
    (is (not (app/open? ov :d)) "starts closed")
    (app/open! ov :d {:id 42})
    (is (app/open? ov :d))
    (is (= {:id 42} (app/overlay-arg ov :d)) "payload round-trips through the map")
    (app/open! ov :help)
    (is (= true (app/overlay-arg ov :help)) "opened with no arg carries true")
    (is (app/open? ov :d) "a second overlay does not close the first (named, stackable)")
    (app/toggle! ov :d)
    (is (not (app/open? ov :d)) "toggle closes an open one")
    (app/close-all! ov)
    (is (not (app/open? ov :help)) "close-all! clears everything")))

(defn- frame-html
  [h]
  (loop [h h]
    (cond (nil? h) nil
          (#{ui/drawer ui/dialog} (first h))
          (let [{:keys [class title]} (second h)]
            (str class " " title " " (rds/render-to-string (into [:div] (drop 2 h)))))
          (fn? (first h)) (recur (apply (first h) (rest h)))
          :else (rds/render-to-string h))))

(deftest overlay-renders-only-while-open
  (let [ov (ctrl)
        el (fn [] [app/overlay ov :d {:kind :drawer :title "Auftrag" :side :right}
                   (fn [arg] [:span "id=" (str (:id arg))])])]
    (is (nil? (frame-html (el))) "closed → renders nothing")
    (app/open! ov :d {:id 7})
    (let [html (frame-html (el))]
      (is (re-find #"capp-drawer--right" html) "drawer chrome painted on the right")
      (is (re-find #"Auftrag" html) "title rendered")
      (is (re-find #"id=(<!-- -->)?7" html) "content fn received the open! payload"))))

(deftest vars-graph-propagates-through-derived
  (let [rows [{:r :nord :amt 10} {:r :ost :amt 5} {:r :nord :amt 7}]
        g (app/vars
           {:region {:default :all}
            :rows   {:derived (fn [ref]
                                (let [r @(ref :region)]
                                  (if (= r :all) rows (filter #(= (:r %) r) rows))))}
            :total  {:derived (fn [ref] (reduce + (map :amt @(ref :rows))))}})]
    (is (= 22 @(:total g)) ":all → sum of every row")
    (reset! (:region g) :nord)
    (is (= 2 (count @(:rows g))) "a state change recomputes the dependent :derived")
    (is (= 17 @(:total g)) "and cascades through a second :derived automatically")
    (swap! (:region g) (constantly :ost))
    (is (= 5 @(:total g)) "swap! on the state var propagates too")))

(deftest vars-url-backed-state-var
  (let [g (app/vars {:tab {:default :overview :external-id :tab
                           :in-url :always :as :keyword}})]
    (is (= :overview @(:tab g)) "an addressed var is a view-state handle with its default")
    (reset! (:tab g) :orders)
    (is (= :orders @(:tab g)) "and is settable like any state var")))

(deftest only-an-external-id-reaches-the-address-bar
  (testing "a variable with no :external-id is the app's own state, never a URL cursor"
    (let [g (app/vars {:tab {:default :overview :in-url :always :as :keyword}})]
      (is (not (instance? vs/ViewCursor (:tab g)))
          "it still works — it just is not addressable, so its internal name cannot leak")
      (reset! (:tab g) :orders)
      (is (= :orders @(:tab g)))))
  (testing "an embedded module does not bind to its host's address bar"
    (let [g (app/vars {:tab {:default :overview :external-id :tab
                             :in-url :always :as :keyword}}
                      false)]
      (is (not (instance? vs/ViewCursor (:tab g)))
          "embedding carries over no routing configuration")
      (reset! (:tab g) :orders)
      (is (= :orders @(:tab g)))))
  (testing "a declared one in a top-level module does"
    (let [g (app/vars {:tab {:default :overview :external-id :tab
                             :in-url :always :as :keyword}})]
      (is (instance? vs/ViewCursor (:tab g)))))
  (testing ":in-url :never reads the address once and is a plain atom after"
    (let [g (app/vars {:tab {:default :overview :external-id :tab
                             :in-url :never :as :keyword}})]
      (is (not (instance? vs/ViewCursor (:tab g)))
          "a value handed in at load belongs to the app from then on"))))

(deftest vars-unknown-reference-is-loud
  (let [g (app/vars {:bad {:derived (fn [ref] @(ref :nope))}})]
    (is (thrown? js/Error @(:bad g))
        "referencing a var that isn't in the graph throws, not silently nil")))

(deftest tabs-bind-to-a-handle-and-mark-active
  (let [sel  (vs/make (atom {}) :tab :overview {:as :keyword} (fn [_ _] nil))
        _    (reset! sel :orders)
        html (rds/render-to-string
              [(fn [] [app/tabs sel [{:id :overview :label "Übersicht"}
                                     {:id :orders :label "Aufträge"}]])])]
    (is (re-find #"aria-selected=\"true\"[^>]*>(<!-- -->)?Aufträge" html)
        "the handle's value marks its tab active")
    (is (not (re-find #"aria-selected=\"true\"[^>]*>(<!-- -->)?Übersicht" html))
        "the inactive tab has no active modifier")))

(deftest filter-pills-toggle-a-set-handle
  (let [sel (vs/make (atom {}) :f #{} {:as :edn} (fn [_ _] nil))]
    (testing "toggling membership in the set handle"
      (swap! sel conj :offen)
      (is (contains? @sel :offen))
      (swap! sel disj :offen)
      (is (not (contains? @sel :offen))))
    (testing "active value → active pill; count + all-label render"
      (reset! sel #{:offen})
      (let [html (rds/render-to-string
                  [(fn [] [app/filter-pills sel
                           [{:value :offen :label "Offen" :count 12}
                            {:value :zu :label "Erledigt"}]
                           {:all-label "Alle"}])])]
        (is (re-find #"bp-intent-primary[^>]*>(?:<[^>]+>)*Offen" html) "selected pill is active")
        (is (re-find #"Offen 12" html) "count rendered")
        (is (re-find #"Alle" html) "clear-all pill rendered")
        (is (not (re-find #"bp-intent-primary[^>]*>(?:<[^>]+>)*Alle" html))
            "clear-all is inactive while the set is non-empty")))))

(deftest i18n-translates-with-fallback
  (let [t (app/i18n {:de {:orders "Aufträge"} :en {:orders "Orders" :help "Help"}} :de)]
    (is (= "Aufträge" (t :orders)) "hits the pinned locale")
    (is (= "Help" (t :help)) "falls back to :en when :de lacks the key")
    (is (= "missing" (t :missing)) "unknown key degrades to its name, never blank")))

(deftest pivot-table-cross-tabs-and-totals
  (let [rows [{:reg :nord :q :q1 :amt 10} {:reg :nord :q :q2 :amt 5}
              {:reg :ost  :q :q1 :amt 3}  {:reg :ost  :q :q2 :amt 7}
              {:reg :nord :q :q1 :amt 2}]
        html (rds/render-to-string
              [(fn [] [app/pivot-table {:rows rows :row :reg :col :q :value :amt :agg :sum
                                        :corner "Region"}])])]
    (is (re-find #"capp-pivot" html))
    (is (re-find #"Region" html) "corner header")
    (is (re-find #">12<" html) "a cell aggregates its group (nord/q1 = 12)")
    (is (re-find #">27<" html) "grand total present")
    (is (re-find #"capp-pivot__tot" html) "row totals rendered"))
  (testing ":count agg"
    (let [html (rds/render-to-string
                [(fn [] [app/pivot-table {:rows [{:a 1 :b 1} {:a 1 :b 1} {:a 2 :b 1}]
                                          :row :a :col :b :agg :count :totals? false}])])]
      (is (re-find #">2<" html) "a=1,b=1 has 2 items")
      (is (re-find #">1<" html) "a=2,b=1 has 1 item")))
  (testing "an OBJECT-bucket key (engine group-by [[:link …]]) labels by :title, groups by :id"
    (let [rows [{:mit {:id "m1" :title "Mustermann"} :q :q1 :amt 10}
                {:mit {:id "m1" :title "Mustermann"} :q :q1 :amt 5}
                {:mit {:id "m2" :title "Sandmeier"}    :q :q1 :amt 3}]
          html (rds/render-to-string
                [(fn [] [app/pivot-table {:rows rows :row :mit :col :q :value :amt
                                          :agg :sum :totals? false}])])]
      (is (re-find #"Mustermann" html) "the row header shows the TITLE, not the id")
      (is (re-find #"Sandmeier" html))
      (is (re-find #">15<" html) "the two Mustermann rows fold by :id (10+5=15)")))
  (testing "the DSL registry reads a group dim / measure through v off a namespaced agg row"
    (let [rows [{:group {:mit {:id "m1" :title "Mustermann"} :q "Q1"} :metrics {:amt 15}}
                {:group {:mit {:id "m2" :title "Sandmeier"}    :q "Q1"} :metrics {:amt 3}}]
          node ((get render/widget-registry :pivot-table)
                {:rows rows :row :mit :col :q :value :amt :agg :sum :totals? false})
          html (rds/render-to-string [(fn [] node)])]
      (is (re-find #"Mustermann" html) "the linked object's title reaches the pivot via v")
      (is (re-find #">15<" html) "and its measure folds correctly through the split shape"))))

(defn- lrender [tree] (rds/render-to-string [(fn [] (app/layout tree))]))

(deftest layout-tabs-bound-to-handle
  (let [sel (vs/make (atom {}) :t :a {:as :keyword} (fn [_ _] nil))
        tree {:kind :tabs :select sel
              :tabs [{:id :a :label "A" :body [:div "AA"]}
                     {:id :b :label "B" :body [:div "BB"]}]}]
    (is (re-find #"AA" (lrender tree)) "active tab's body shown")
    (is (not (re-find #"BB" (lrender tree))) "inactive tab's body hidden")
    (reset! sel :b)
    (is (re-find #"BB" (lrender tree)) "switching the handle swaps the body")))

(deftest layout-pages-header-persists
  (let [sel (vs/make (atom {}) :p :o {:as :keyword} (fn [_ _] nil))
        html (lrender {:kind :pages :select sel
                       :header {:title "Mein Modul" :items [[:button "Export"]]}
                       :pages [{:id :o :label "Übersicht" :body [:div "OBODY"]}
                               {:id :d :label "Detail" :body [:div "DBODY"]}]})]
    (is (re-find #"capp-l-header" html))
    (is (re-find #"Mein Modul" html) "module title in the persistent header")
    (is (re-find #"Übersicht" html) "page tabs in the header")
    (is (re-find #"Export" html) "header items rendered")
    (is (re-find #"OBODY" html) "active page body")
    (is (not (re-find #"DBODY" html)) "other page body hidden")))

(deftest layout-variable-backed-visibility-and-collapse
  (testing ":when hides a node"
    (is (not (re-find #"SECRET"
                      (lrender {:kind :rows :when (atom false) :items [[:div "SECRET"]]}))))
    (is (re-find #"SHOWN" (lrender {:kind :rows :when (atom true) :items [[:div "SHOWN"]]}))))
  (testing ":collapse hides the body but keeps the title"
    (let [html (lrender {:kind :rows :title "Abschnitt" :collapse (atom true)
                         :items [[:div "HIDDENBODY"]]})]
      (is (re-find #"Abschnitt" html) "title still shown")
      (is (not (re-find #"HIDDENBODY" html)) "collapsed body hidden")
      (is (re-find #"▸" html) "collapsed toggle glyph"))))

(deftest layout-loop-one-instance-per-object
  (let [objs [{:id 1 :name "Alma"} {:id 2 :name "Bente"} {:id 3 :name "Cosmo"}]
        html (lrender {:kind :loop :items objs :arrange :grid
                       :body (fn [o] [:div.card "Kopf: " (:name o)])})]
    (is (re-find #"capp-l-loop--grid" html) "arrange class applied")
    (is (= 3 (count (re-seq #"capp-l-loop__item" html))) "one keyed instance per object")
    (is (every? #(re-find % html) [#"Alma" #"Bente" #"Cosmo"]) "body rendered per object")))

(deftest layout-loop-marks-the-selected-row
  (let [sel  (atom "2")
        html (lrender {:kind :loop :key :id :selected sel
                       :items [{:id "1" :name "Alma"} {:id "2" :name "Bente"}]
                       :body (fn [o] [:div (:name o)])})]
    (is (= 1 (count (re-seq #"capp-l-loop__item--selected" html))) "only the selected row")
    (is (re-find #"capp-l-loop__item--selected[^>]*>\s*<div>Bente" html) "it is the keyed row"))
  (testing "without :selected no row is marked"
    (is (not (re-find #"--selected"
                      (lrender {:kind :loop :items [{:id 1}] :body (fn [_] [:div "x"])})))))
  (testing "a row click selects and clears; a click on a control only selects"
    (is (= "1" (render/loop-selection-after-click nil "1" false)))
    (is (nil? (render/loop-selection-after-click "1" "1" false)))
    (is (= "1" (render/loop-selection-after-click "1" "1" true)))
    (is (= "2" (render/loop-selection-after-click "1" "2" true)))))

(deftest layout-loop-empty-fallback
  (let [html (lrender {:kind :loop :items []
                       :body (fn [o] [:div (:name o)])
                       :empty [:div "keine Objekte"]})]
    (is (re-find #"keine Objekte" html) ":empty shows when the set is empty")
    (is (not (re-find #"capp-l-loop__item" html)) "no instances rendered")))

(deftest layout-loop-nests-a-container-body
  (let [html (lrender {:kind :loop :items [{:id 1 :name "X"}]
                       :body (fn [o] {:kind :section :title (:name o)
                                      :body [:div "inner"]})})]
    (is (re-find #"capp-l-section__title" html) "a loop body can be a full container node")
    (is (re-find #">X<" html))
    (is (re-find #"inner" html))))

(deftest explicit-parse-encode-overrides-preset
  (testing ":parse/:encode beat :as — e.g. a comma-joined id list"
    (let [{:keys [cur store]}
          (handle :ids []
                  {:parse #(if (seq %) (vec (.split % ",")) [])
                   :encode #(when (seq %) (str/join "," %))})]
      (reset! cur ["a" "b" "c"])
      (is (= ["a" "b" "c"] @cur))
      (is (= "a,b,c" (get @store "ids")) "a readable, shareable list in the URL"))))

(deftest cursor-is-watchable-like-an-atom
  (testing "add-watch fires on a change to THIS key, with old/new parsed values"
    (let [{:keys [cur]} (handle :tab :overview {:as :keyword})
          seen (atom [])]
      (add-watch cur ::t (fn [k _ o n] (swap! seen conj [k o n])))
      (reset! cur :orders)
      (is (= [[::t :overview :orders]] @seen)
          "the watcher sees parsed VALUES, not the raw URL strings")))
  (testing "a change to a DIFFERENT key does not fire this cursor's watch"
    (let [{:keys [cur store]} (handle :tab :overview {:as :keyword})
          seen (atom 0)]
      (add-watch cur ::t (fn [_ _ _ _] (swap! seen inc)))
      (swap! store assoc "other" "x")
      (is (zero? @seen) "sibling params share one store but not each other's watches")))
  (testing "a store reset! (what a popstate does) fires the watch"
    (let [{:keys [cur store]} (handle :tab :overview {:as :keyword})
          seen (atom [])]
      (add-watch cur ::t (fn [_ _ o n] (swap! seen conj [o n])))
      (reset! store {"tab" "orders"})
      (is (= [[:overview :orders]] @seen) "Back/Forward moves watched state too")))
  (testing "remove-watch stops it"
    (let [{:keys [cur]} (handle :tab :overview {:as :keyword})
          seen (atom 0)]
      (add-watch cur ::t (fn [_ _ _ _] (swap! seen inc)))
      (remove-watch cur ::t)
      (reset! cur :orders)
      (is (zero? @seen))))
  (testing "two watchers on the same cursor are independent"
    (let [{:keys [cur]} (handle :tab :overview {:as :keyword})
          a (atom 0) b (atom 0)]
      (add-watch cur ::a (fn [_ _ _ _] (swap! a inc)))
      (add-watch cur ::b (fn [_ _ _ _] (swap! b inc)))
      (remove-watch cur ::a)
      (reset! cur :orders)
      (is (= [0 1] [@a @b]) "removing one watcher leaves the other armed"))))
