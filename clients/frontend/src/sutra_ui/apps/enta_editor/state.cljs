(ns sutra-ui.apps.enta-editor.state
  (:require [reagent.ratom :as ratom]
            [cljs.reader :as reader]
            [cljs.pprint :as pprint]
            [clojure.string :as str]
            [sutra.capps.catalog :as cat]
            [sutra.capps.derived :as cd]
            [sutra.capps.editing :as ed]
            [sutra.capps.tree-move :as tm]
            [sutra-ui.apps.enta-editor.events :as ev]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]
            [sutra.capps.table :as tbl]))

(defonce ui (shell/slice :app-builder))

(defonce builder (machine/frame ui {:name :app-builder}))

(defn send! [event] (machine/dispatch event {:frame builder}))

(defonce draft (ratom/make-reaction #(:draft @ui)))

(defn- cur [] (or @draft ev/blank-draft))

(machine/reg-sub :enta.ui/at (fn [db [_ k]] (get db k (get ev/ui-defaults k))))

(defn ui-get
  [k]
  @(machine/subscribe [:enta.ui/at k] {:frame builder}))

(defn ui-set! [m] (send! [:enta.ui/set m]))

(defn ui-flip! [k x] (send! [:enta.ui/flip k x]))

(defn pane [] (ui-get :pane))

(defn left [] (ui-get :left))

(defn prop-display-of
  [object-type]
  (into {}
        (for [{:keys [name type opts]} (get-in @catalog/schema [:object-types object-type :fields])]
          [name (assoc (tbl/prop-display {:type type :name (:name opts) :format (:format opts)}
                                         name)
                       :type       type
                       :visibility (:visibility opts))])))

(defn- object-view-app?
  [kw spec]
  (boolean (or (:form-factor spec)
               (re-find #"-view-(full|panel)$" (name kw)))))

(defn app-options []
  (vec (sort-by (comp str/lower-case :label)
                (for [[kw spec] (get @catalog/canonical-schema :applications {})
                      :when (:id spec)]
                  {:id kw :rid (str (:id spec))
                   :view? (object-view-app? kw spec)
                   :label (str (or (:display-name spec) (:name spec) (name kw)))}))))

(def recent-apps ev/recent-apps)

(defn env
  []
  {:types (keys (:objects @catalog/schema))
   :rid   (get-in @catalog/canonical-schema [:applications (:app (cur)) :id])})

(defn load-app! [app-kw] (send! [:enta/load app-kw]))

(defn restore-file!
  [file]
  (send! [:enta/restore file]))

(defn new-app!
  ([template-id] (new-app! template-id nil))
  ([template-id opts] (send! [:enta/new template-id opts])))

(defn preview-source [] (:preview-source (cur) :none))

(defn preview-error [] (:preview-error (cur)))

(defn refresh-preview! [] (send! [:enta.preview/refresh (:rid (env))]))

(defn preview-shown! [shown?] (send! [:enta.preview/shown shown? (:rid (env))]))

(defn intend!
  ([id] (intend! id nil))
  ([id args] (send! [:enta/intend id args (env)]) nil))

(defn tree [] (:tree (cur)))
(defn cursor [] (:cursor (cur)))
(defn node-at-cursor [] (ed/node (cur)))

(defn select! [path] (intend! :select {:path path}))

(defn run-at!
  ([path id] (run-at! path id nil))
  ([path id args] (select! path) (intend! id args)))

(defn can-undo? [] (ed/can-undo? (cur)))
(defn can-redo? [] (ed/can-redo? (cur)))
(defn undo-label [] (ed/undo-label (cur)))
(defn redo-label [] (ed/redo-label (cur)))
(defn undo! [] (intend! :undo))
(defn redo! [] (intend! :redo))

(defn available [] (ed/available (cur)))
(defn refusal
  ([id] (ed/refusal (cur) id nil))
  ([id args] (ed/refusal (cur) id args)))

(defn on-chord!
  [chord]
  (when-let [id (get ed/by-key chord)]
    (when (ed/possible? (cur) id)
      (intend! id)
      id)))

(defn mode [] (:mode (cur)))
(defn pending [] (:pending (cur)))

(defn mode!
  ([event] (mode! event nil))
  ([event payload] (send! [:enta/mode event payload]) nil))

(defn put-param! [path k v] (intend! :put-param {:path path :key k :value v}))
(defn replace-node! [path node] (intend! :replace-node {:path path :node node}))
(defn add! [parent-path slot kind] (intend! :add {:path parent-path :slot slot :kind kind}))
(defn add-at-cursor! [kind] (intend! :add {:kind kind}))
(defn container-target [] (ed/target (cur)))

(defn remove! [path] (run-at! path :remove))
(defn duplicate! [path] (run-at! path :duplicate))
(defn retype! [path widget] (run-at! path :retype {:widget widget}))

(defn drop-plan [src tgt zone] (tm/plan (tree) (vec src) (vec tgt) zone))
(defn drop-target [path zone] (tm/target (tree) (vec path) zone))
(defn movable? [path] (tm/movable? (tree) (vec path)))
(defn move-to! [src tgt zone] (intend! :move-to {:src src :tgt tgt :zone zone}) true)
(defn add-at! [path zone kind] (intend! :add {:path path :zone zone :kind kind}))

(defn clip-label [] (:label (:clip (cur))))
(defn can-paste? [] (some? (:node (:clip (cur)))))
(defn paste-at! [path] (intend! :paste {:path path}) true)
(defn paste! [] (intend! :paste))

(defn put-var! [path nm spec] (intend! :put-var {:path path :name nm :spec spec}))
(defn rename-var! [path old new] (intend! :rename-var {:path path :from old :to new}))
(defn drop-var! [path nm] (intend! :drop-var {:path path :name nm}))

(defn fresh-var-name
  [words]
  (cat/var-slug words (cat/declared-vars (tree))))

(defn put-param-with-var!
  [path k ref-key nm spec]
  (intend! :bind-new-var {:path path :key k :ref ref-key :name nm :spec spec}))

(defn file [] (:file (cur)))

(defn put-file-key! [k v] (intend! :put-file-key {:key k :value v}))

(defn object-types [] (vec (:object-types (file))))

(defn set-object-types! [ts] (put-file-key! :object-types (vec ts)))

(defn derived-module
  []
  (:derived-properties (file)))

(defn- set-derived-module! [m] (put-file-key! :derived-properties m))

(defn put-derived!    [type nm spec] (set-derived-module! (cd/put (derived-module) type nm spec)))
(defn drop-derived!   [type nm]
  (set-derived-module! (cd/drop-property (derived-module) type nm)))
(defn rename-derived! [type old new]
  (set-derived-module! (cd/rename (derived-module) type old new)))

(defn problems [] (cat/explain-tree (tree)))

(defn preview-descriptor
  []
  (merge (:consumer (cur))
         (select-keys (:file (cur)) [:name :icon :description])
         {:layout (cat/instrument (tree))}))

(defn app-key [] (ev/app-key (cur)))

(defn enta-file [] (ev/enta-file (cur)))

(defn enta-text [] (with-out-str (pprint/pprint (enta-file))))

(defn dirty?
  []
  (let [{:keys [saved file tree]} (cur)]
    (and (some? tree) (not= saved [file tree]))))

(defn stage!
  []
  (send! [:enta/stage]))

(defn read-edn [s]
  (try {:value (reader/read-string s)}
       (catch :default e {:error (or (.-message e) (str e))})))

(defn copy-enta! []
  (when-let [clip (some-> js/navigator .-clipboard)]
    (.writeText clip (enta-text))))
