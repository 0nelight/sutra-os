(ns sutra.cli.verbs.graphs
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.live :refer [commit-unless-staged! find-or-create-folder! kompass-nodes!]]
            [sutra.cli.out :refer [emit! mark paint render say!]]
            [sutra.i18n.core :as i18n]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.types.rid :as rid]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [sutra.cli.view :refer
             [busy-graph graph-node-refs kw-name ref-tokens scope-count]]))

(defn- graph-handle!
  [handle]
  (let [g (req! (str "graph " handle) (api/get! :graph-open {:handle (str handle)}))]
    (when-not (:name g)
      (throw (ex-info (str "no graph " handle
                           " — `lineage-graph list` shows title, path and rid for each")
                      {:graph handle})))
    g))

(def ^:private scope-kinds
  {:datasources "datasource" :datasets "dataset" :transforms "transform"
   :object-types "object-type" :link-types "link-type" :actions "action"
   :functions "function" :applications "application"})

(defn- expand-scope
  [vt up down]
  (let [anchors (for [[k ns] vt :let [kind (scope-kinds k)] :when kind n ns]
                  (str kind "/" (kw-name n)))
        bucket  (set/map-invert scope-kinds)]
    (if (empty? anchors)
      vt
      (->> (:nodes (req! "lineage facts"
                         (api/get! :lineage-facts {:node (str/join "," anchors) :io-only true
                                                   :up up :down down})))
           vals
           (reduce (fn [m {:keys [kind name]}]
                     (if-let [k (bucket kind)] (update m k (fnil conj #{}) (keyword name)) m))
                   vt)))))

(defn- expanded-scope
  [ctx vt {:keys [ancestors descendants]}]
  (let [depth (fn [v] (cond (not v) "0"
                            (or (true? v) (= "" v)) "all"
                            :else (str v)))
        out   (if (or ancestors descendants)
                (expand-scope vt (depth ancestors) (depth descendants))
                vt)]
    (when (or ancestors descendants)
      (let [n (scope-count out)]
        (say! ctx (str "    "
                       (paint :dim
                              (str "expand · " (scope-count vt) " → "
                                   (i18n/t :cli/n-nodes {:n n})))))
        (when (> n busy-graph)
          (say! ctx (str "    " (paint :yellow (i18n/t :cli/too-many-nodes {:n n}))
                         (paint :dim (str " — " (i18n/t :cli/smaller-ancestors))))))))
    out))

(defn- graph-rid!
  [title]
  (:id (graph-handle! (clojure.core/name (rid/slug-kw title)))))

(defn- place-graph!
  [ctx grid into as]
  (let [parent (when into (find-or-create-folder! (kompass-nodes!) (clojure.core/name into)))]
    (req! "place graph" (api/post! :kompass-place (cond-> {:ref grid :parent-rid parent}
                                                    as (assoc :name (clojure.core/name as)))))
    (say! ctx (mark :green "✓" (str (i18n/t :cli/filed-at) " "
                                    (paint :cyan
                                           (str "/" (when into (str (clojure.core/name into) "/"))
                                                (clojure.core/name (or as grid)))))))))

(defmethod step! :graph-list [ctx [_ {:keys [mode project area]}]]
  (let [gs (:graphs (req! "graphs" (api/list-graphs
                                    (cond-> {} mode (assoc :mode mode)
                                            project (assoc :project project)
                                            area    (assoc :area area)))))]
    (emit! ctx {:graphs (vec gs)}))
  nil)

(defmethod step! :graph-show [ctx [_ {:keys [graph]}]]
  (emit! ctx (graph-handle! graph))
  nil)

(defn- draw-graph!
  [ctx fmt graph out layout richtung]
  (let [g    (graph-handle! graph)
        {:keys [status ^bytes bytes]}
        (tx/get-bytes! (paths/address fmt
                                      (cond-> {:id (:id g)}
                                        layout   (assoc :layout (clojure.core/name layout))
                                        richtung (assoc :richtung (clojure.core/name richtung)))))]
    (if-not (<= 200 status 299)
      (throw (ex-info (str "graph " (name fmt) " " graph " → HTTP " status)
                      {:graph graph :status status}))
      (let [text (String. bytes "UTF-8")]
        (if (str/blank? (str out))
          (do (emit! ctx {:graph (:id g) :format (name fmt) :text text} ::drawn)
              {:kind fmt :graph (:id g)})
          (let [path (.getPath (io/file out))]
            (spit path text)
            (say! ctx (mark :green "✓"
                            (i18n/t :cli/wrote-bytes {:n (count bytes) :pfad (paint :bold path)})))
            {:kind :file :path path}))))))

(defmethod render ::drawn [_ {:keys [text]}] (str text))

(defmethod step! :graph-svg [ctx [_ {:keys [graph out layout richtung]}]]
  (draw-graph! ctx :graph-svg graph out layout richtung))

(defmethod step! :graph-dot [ctx [_ {:keys [graph out layout richtung]}]]
  (draw-graph! ctx :graph-dot graph out layout richtung))

(defmethod step! :graph-create
  [ctx [_ {:keys [title mode into project branch fallback area description node nodes stage?]
           :as opts}]]
  (let [refs   (ref-tokens node nodes)
        vt     (expanded-scope ctx (graph-node-refs refs) opts)
        fb     (when fallback (vec (remove str/blank? (str/split (str fallback) #","))))
        body   (cond-> {:name title :title title :mode (or mode :lineage)}
                 area          (assoc :area area)
                 description   (assoc :description description)
                 project       (assoc :project (keyword (clojure.core/name project)))
                 branch        (assoc :branch branch)
                 (seq fb)      (assoc :fallback-branches fb)
                 (seq vt)      (assoc :visible-types vt))]
    (req! "create graph" (api/mutate :create-graph body {:stage? true}))
    (say! ctx (mark :green "✓" (str (i18n/t :cli/saved-graph {:titel (paint :bold title)})
                                    (when (seq refs) (str " · " (scope-count vt) " node"
                                                          (when (not= 1 (scope-count vt)) "s"))))))
    (commit-unless-staged! ctx stage?
                           (str "committed — `lineage-graph show "
                                (clojure.core/name (rid/slug-kw title)) "`"))
    (when into
      (if stage?
        (say! ctx (str "    " (paint :yellow (i18n/t :cli/not-filed-yet))
                       (paint :dim (str " — `commit`, then `lineage-graph move "
                                        (clojure.core/name (rid/slug-kw title))
                                        " --into " (clojure.core/name into) "`"))))
        (place-graph! ctx (graph-rid! title) into title))))
  nil)

(defmethod step! :graph-save-as [ctx [_ {:keys [graph title into stage?]}]]
  (let [src (graph-handle! graph)
        body (cond-> {:name title :title title :mode (:mode src)}
               (:area src)              (assoc :area (:area src))
               (:description src)       (assoc :description (:description src))
               (:project src)           (assoc :project (:project src))
               (:branch src)            (assoc :branch (:branch src))
               (seq (:fallback-branches src))
               (assoc :fallback-branches (vec (:fallback-branches src)))
               (seq (:visible-types src))     (assoc :visible-types (:visible-types src))
               (:payload src)           (assoc :payload (:payload src)))]
    (req! "save-as" (api/mutate :create-graph body {:stage? true}))
    (say! ctx (mark :green "✓"
                    (i18n/t :cli/saved-as
                            {:quelle (paint :bold (:title src)) :ziel (paint :bold title)})))
    (say! ctx (str "    " (paint :dim (i18n/t :cli/positions-not-copied))))
    (commit-unless-staged! ctx stage?
                           (str "committed — `lineage-graph show "
                                (clojure.core/name (rid/slug-kw title)) "`"))
    (when into
      (if stage?
        (say! ctx (str "    " (paint :yellow (i18n/t :cli/not-filed-yet))
                       (paint :dim (str " — `commit`, then `lineage-graph move "
                                        (clojure.core/name (rid/slug-kw title))
                                        " --into " (clojure.core/name into) "`"))))
        (place-graph! ctx (graph-rid! title) into title))))
  nil)

(defmethod step! :graph-rename [ctx [_ {:keys [graph title stage?]}]]
  (let [g (graph-handle! graph)]
    (req! "rename graph"
          (api/mutate :update-graph
                      (cond-> {:name (kw-name (:name g)) :title title :mode (:mode g)}
                        (:area g)                    (assoc :area (:area g))
                        (:description g)             (assoc :description (:description g))
                        (:branch g)                  (assoc :branch (:branch g))
                        (seq (:fallback-branches g))
                        (assoc :fallback-branches (vec (:fallback-branches g)))
                        (seq (:visible-types g))     (assoc :visible-types (:visible-types g))
                        (:payload g)                 (assoc :payload (:payload g)))
                      {:stage? true}))
    (say! ctx (mark :green "✓"
                    (str (i18n/t :cli/retitled
                                 {:alt (paint :bold (:title g)) :neu (paint :bold title)})
                         (paint :dim (str " (rid " (:id g) " unchanged)")))))
    (commit-unless-staged! ctx stage? (i18n/t :cli/committed))
    (when-not stage?
      (when-let [node (->> (kompass-nodes!)
                           (filter #(= (:id g) (:ref %)))
                           first)]
        (req! "rename placement" (api/post! :kompass-node-rename {:rid (:rid node)} {:to title}))
        (say! ctx (str "    " (paint :dim (str (i18n/t :cli/filed-name-updated) " → "
                                               (:path (graph-handle! (:id g))))))))))
  nil)

(defmethod step! :graph-move [ctx [_ {:keys [graph into]}]]
  (let [g (graph-handle! graph)]
    (place-graph! ctx (:id g) into (:title g))
    (say! ctx (str "    " (paint :dim (i18n/t :cli/project-files-hint)))))
  nil)

(defmethod step! :graph-scope [ctx [_ {:keys [graph add remove stage? ancestors descendants]
                                       :as opts}]]
  (let [g       (graph-handle! graph)
        cur     (or (:visible-types g) {})
        plus    (graph-node-refs (ref-tokens add))
        minus   (graph-node-refs (ref-tokens remove))
        merged  (-> (reduce-kv (fn [m k v] (update m k (fnil into #{}) v)) cur plus)
                    (expanded-scope ctx opts))
        final   (reduce-kv (fn [m k v]
                             (let [left (set/difference (set (get m k)) (set v))]
                               (if (seq left) (assoc m k left) (dissoc m k))))
                           merged minus)]
    (when-not (or (seq add) (seq remove) ancestors descendants)
      (throw
       (ex-info
        "lineage-graph scope needs --add/--remove <kind>/<name>, or --ancestors/--descendants [N]"
        {:graph graph})))
    (req! "scope graph"
          (api/mutate :update-graph
                      (cond-> {:name (kw-name (:name g)) :title (:title g) :mode (:mode g)
                               :visible-types final}
                        (:area g)                    (assoc :area (:area g))
                        (:description g)             (assoc :description (:description g))
                        (:branch g)                  (assoc :branch (:branch g))
                        (seq (:fallback-branches g))
                        (assoc :fallback-branches (vec (:fallback-branches g)))
                        (:payload g)                 (assoc :payload (:payload g)))
                      {:stage? true}))
    (emit! ctx {:graph (:title g) :nodes (scope-count final) :visible-types final})
    (commit-unless-staged! ctx stage? (i18n/t :cli/committed)))
  nil)

(defmethod step! :graph-branch [ctx [_ {:keys [graph branch fallback stage?]}]]
  (let [g  (graph-handle! graph)
        fb (when fallback (vec (remove str/blank? (str/split (str fallback) #","))))]
    (when-not (or branch fallback)
      (throw (ex-info (i18n/t :cli/graph-branch-needs-flags) {:graph graph})))
    (req! "graph branch"
          (api/mutate :update-graph
                      (cond-> {:name (kw-name (:name g)) :title (:title g) :mode (:mode g)
                               :branch (or branch (:branch g))}
                        (seq fb)                     (assoc :fallback-branches fb)
                        (and (nil? fb) (seq (:fallback-branches g)))
                        (assoc :fallback-branches (vec (:fallback-branches g)))
                        (:area g)                    (assoc :area (:area g))
                        (:description g)             (assoc :description (:description g))
                        (seq (:visible-types g))     (assoc :visible-types (:visible-types g))
                        (:payload g)                 (assoc :payload (:payload g)))
                      {:stage? true}))
    (say! ctx (mark :green "✓" (str (i18n/t :cli/branch-config-of
                                            {:titel (paint :bold (:title g))
                                             :zweig (or branch (:branch g) "—")})
                                    (when (seq fb) (str "  fallback " (str/join " → " fb))))))
    (commit-unless-staged! ctx stage? (i18n/t :cli/committed)))
  nil)

(def ^:private arrangements
  #{:hier-lr :hier-tb :clustered :grid :circular :force :tree})

(defmethod step! :graph-arrange [ctx [_ {:keys [graph arrangement stage?]}]]
  (let [g   (graph-handle! graph)
        a   (keyword (kw-name arrangement))
        off (= :none a)]
    (when-not (or off (arrangements a))
      (throw (ex-info (str "no arrangement " (kw-name a) " — one of "
                           (str/join ", " (map kw-name (sort arrangements))) ", none")
                      {:graph graph :arrangement a})))
    (req! "graph arrangement"
          (api/mutate :update-graph
                      (cond-> {:name (kw-name (:name g)) :title (:title g) :mode (:mode g)
                               :payload (if off
                                          (dissoc (:payload g) :arrangement)
                                          (assoc (:payload g) :arrangement a))}
                        (:area g)                    (assoc :area (:area g))
                        (:description g)             (assoc :description (:description g))
                        (:branch g)                  (assoc :branch (:branch g))
                        (seq (:fallback-branches g))
                        (assoc :fallback-branches (vec (:fallback-branches g)))
                        (seq (:visible-types g))     (assoc :visible-types (:visible-types g)))
                      {:stage? true}))
    (say! ctx (mark :green "✓" (str (paint :bold (:title g)) " opens "
                                    (if off "in the default layout" (str "arranged " (kw-name a)))
                                    (paint :dim " — where no position is saved"))))
    (commit-unless-staged! ctx stage? (i18n/t :cli/committed)))
  nil)

(defmethod step! :graph-delete [ctx [_ {:keys [graph stage?]}]]
  (let [g (graph-handle! graph)]
    (req! "delete graph" (api/mutate :delete-graph {:name (kw-name (:name g))} {:stage? true}))
    (say! ctx (mark :red "−" (str (i18n/t :cli/deleted-graph {:titel (paint :bold (:title g))})
                                  (paint :dim (str " — " (i18n/t :cli/swept-with-it))))))
    (commit-unless-staged! ctx stage? (i18n/t :cli/committed)))
  nil)
