(ns sutra-ui.state.atlas
  (:require [cljs.reader :as edn]
            [reagent.ratom :as ratom]
            [sutra-ui.state.requests :as rq]
            [sutra.kit.remote :as rd]
            [sutra-ui.api :as api]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.shell :as shell]))

(declare touch-project-recent!)

(defonce
  kompass-nodes (db/cursor [:kompass :kompass-nodes] rd/loading))

(defn kompass-node-list [] (rd/value-or @kompass-nodes []))

(def refresh-nodes
  (db/load [:kompass :kompass-nodes] [api/get! :kompass-nodes]
           {:pick (comp vec :nodes) :report "kompass"}))

(defn refresh-kompass-nodes! [] (db/send! (second refresh-nodes)))

(defn kompass-place!
  [ref parent-rid name]
  (db/promise! [:rf/request {:ask     [api/post! :kompass-place
                                       {:ref ref :parent-rid parent-rid :name name}]
                             :fx      [refresh-nodes]
                             :reject? true}]))

(def default-project
  ::default)

(defn select-project!
  [proj-key]
  (shell/edit! assoc :selected-project proj-key)
  (when (and proj-key (not= proj-key default-project)) (touch-project-recent! proj-key)))

(defn- ls-get [k] (try (some-> (.getItem js/localStorage k) edn/read-string)
                       (catch :default _ nil)))

(defn- ls-set! [k v] (try (.setItem js/localStorage k (pr-str v)) (catch :default _ nil)))

(defonce kompass-recents
  (db/cursor [:kompass :kompass-recents] (or (ls-get "sutra.kompass.recents") [])))

(add-watch kompass-recents   ::p (fn [_ _ _ v] (ls-set! "sutra.kompass.recents" v)))

(defonce
  kompass-answer (db/cursor [:kompass :kompass-answer] rd/loading))

(def ^:private kompass-before
  {:modified {:times {}} :rights {} :portfolios [] :collections [] :shared {} :sharing #{}
   :home-folders true})

(def kompass-index
  (ratom/make-reaction #(rd/value-or (rd/fmap (fn [k] (dissoc k :favorites)) @kompass-answer)
                                     kompass-before)))

(def favorites
  (ratom/make-reaction #(rd/fmap :favorites @kompass-answer)))

(defn- with-favorite [db on? ref]
  (update-in db [:kompass :kompass-answer]
             (fn [k] (rd/fmap #(update % :favorites (if on? conj disj) ref) k))))

(defn favorite-of
  [ref]
  (rd/fmap #(contains? % ref) @favorites))

(defn file-favorite? [ref] (true? (rd/value-or (favorite-of ref) false)))

(db/reg :sutra.kompass/favorite
        (fn [db [_ ref]]
          (let [favs (rd/fmap :favorites (get-in db [:kompass :kompass-answer]))]
            (when (rd/value? favs)
              (let [on? (not (contains? favs ref))]
                {:db (with-favorite db on? ref)
                 :fx [[:call {:call [api/post! :kompass-favorites {:ref ref :on on?}]
                              :err [:sutra.kompass/favorite-refused on? ref]}]]})))))

(db/reg :sutra.kompass/favorite-refused
        (fn [db [_ on? ref sentence]]
          {:db (with-favorite db (not on?) ref) :fx [(rq/failure nil sentence)]}))

(defn toggle-file-favorite!
  [ref]
  (db/send! [:sutra.kompass/favorite ref]))

(defn app-favorite-ref [app-name] (str "applications/" (name app-name)))

(defn- carry-over-browser-favorites!
  []
  (when-let [old (seq (ls-get "sutra.kompass.favorites"))]
    (doseq [pkey old
            :let [id (get-in @catalog/canonical-schema [:projects pkey :id])]
            :when id]
      (let [ref (str "project/" id)]
        (when-not (file-favorite? ref) (toggle-file-favorite! ref))))
    (try (.removeItem js/localStorage "sutra.kompass.favorites") (catch :default _ nil))))

(defn- kompass-of
  [r]
  (-> r
      (update :favorites #(set (map str %)))
      (update :sharing #(set (map str %)))
      (update :shared #(into {} (for [[k v] %]
                                  [(if (keyword? k) (subs (str k) 1) (str k)) (keyword v)])))))

(db/reg :sutra.kompass/refresh-index
        (fn [_ [_ opts reply]]
          {:fx [(db/load [:kompass :kompass-answer] [api/get! :kompass-index]
                         (merge {:pick kompass-of :then [[:sutra.kompass/index]] :report true}
                                opts)
                         reply)]}))

(db/reg :sutra.kompass/index
        (fn [_ [_ r]]
          {:fx [(when (get-in r [:modified :computing])
                  [:after {:key :sutra.kompass/index-again :ms 5000
                           :event [:sutra.kompass/refresh-index nil]}])
                [:invoke [carry-over-browser-favorites!]]]}))

(defn refresh-kompass-index!
  ([] (refresh-kompass-index! nil))
  ([opts] (db/promise! [:sutra.kompass/refresh-index opts])))

(defn ensure-favorites!
  []
  (when (rd/loading? @favorites)
    (refresh-kompass-index! {:dedup ::kompass-index :report nil})))

(defn touch-project-recent! [pkey]
  (when pkey
    (db/edit! kompass-recents (fn [xs] (->> (cons pkey (remove #{pkey} xs)) (take 8) vec)))))

(def ^:private om-store-key "sutra.om.discover")

(defn- read-om-store []
  (try (or (some-> (.getItem js/localStorage om-store-key) edn/read-string) {})
       (catch :default _ {})))

(defonce om-discover (db/cursor [:kompass :om-discover] (read-om-store)))

(defn- persist-om-discover! []
  (try (.setItem js/localStorage om-store-key (pr-str @om-discover))
       (catch :default _ nil)))

(defn om-key [] "_")

(defn- same-ref? [a b]
  (and (= (:kind a) (:kind b)) (= (str (:id a)) (str (:id b)))))

(defn om-favorites [] (get-in @om-discover [(om-key) :favorites] []))

(defn note-recent!
  [ref]
  (when (:id ref)
    (db/edit! om-discover update-in [(om-key) :recents]
              (fn [rs] (vec (take 8 (cons ref (remove #(same-ref? % ref) rs))))))
    (persist-om-discover!)))

(defn favorite? [ref]
  (boolean (some #(same-ref? % ref) (om-favorites))))

(defn toggle-favorite! [ref]
  (db/edit! om-discover update-in [(om-key) :favorites]
            (fn [fs] (if (some #(same-ref? % ref) fs)
                       (vec (remove #(same-ref? % ref) fs))
                       (vec (cons ref fs)))))
  (persist-om-discover!))

(def ^:private asks
  {:documents    [[api/documents] :documents]
   :explorations [[api/get! :explorations] :explorations]
   :widget-sets  [[api/get! :widget-sets] :widget-sets]
   :files        [[api/get! :kompass-files] :files]})

(defonce
  lists
  (into {} (map (fn [k] [k (db/cursor [:kompass :lists k] rd/loading)])) (keys asks)))

(defn rows
  [family]
  (rd/value-or @(lists family) []))

(defn refresh
  [family]
  (let [[ask k] (asks family)]
    (db/load [:kompass :lists family] ask {:pick (comp vec k) :report true})))

(db/reg :sutra.kompass/refresh
        (fn [_ [_ families]] {:fx (mapv refresh families)}))

(defn refresh!
  ([] (db/send! [:sutra.kompass/refresh (keys asks)]))
  ([family] (db/send! [:sutra.kompass/refresh [family]])))

(db/reg :sutra.kompass/exploration
        (fn [db [_ rid body]]
          (rq/request db {:ask  [api/put! :exploration {:id rid} body]
                          :then [[:sutra.kompass/refresh [:explorations]]] :report true})))

(defn make-exploration-private! [rid]
  (db/send! [:sutra.kompass/exploration rid {:private true}]))

(defn rename-exploration! [rid nm]
  (db/send! [:sutra.kompass/exploration rid {:name nm}]))

(defn- file-ref [family id]
  (case family
    :documents    (str "document/" id)
    :widget-sets  (str "widget-set/" id)
    :explorations (str "exploration/" id)
    :files        (str "file/" id)
    (str id)))

(defn move-file-to-project!
  ([family id project]
   (move-file-to-project! family id project shell/fail!))
  ([family id project on-error]
   (-> (db/promise! [:rf/request {:ask     [api/post! :kompass-place
                                            {:ref     (file-ref family id)
                                             :project (some-> project clojure.core/name)}]
                                  :fx      (conj (mapv refresh (keys asks)) refresh-nodes)
                                  :reject? true}])
       (.catch (fn [e] (on-error e) nil)))))
