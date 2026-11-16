(ns sutra.cli.verbs.read
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.live :refer [backing*]]
            [sutra.cli.out :refer [emit! grid mark paint prompt-yes? render say! show-lines warn!]]
            [sutra.i18n.core :as i18n]
            [clojure.string :as str]
            [sutra.wire.api :as api]
            [sutra.cli.view :as view :refer
             [latest-order object-type-entry plural project-rows query-columns review-glyph
              row-class row-name row-verb verb-codes]]))

(declare default-show-cols object-primary-key title-key-hint)

(defn- review-lines
  [staged title verbose?]
  (let [n (count staged)]
    (flatten
     [(paint :bold (str "  ── " (i18n/t :cli/staged) (when title (str ": " title))
                        " — " (i18n/t :cli/n-changes {:n n}) " ──"))
      (if (zero? n)
        (paint :dim (str "     (" (i18n/t :cli/nothing-staged) ")"))
        (for [[[verb cls] rows] (->> staged (group-by (juxt row-verb row-class)) (sort-by key))
              :let [codes (verb-codes verb :gray)
                    glyph (review-glyph verb "•")
                    names (->> rows (map row-name) (remove #{"" "?"}) distinct sort)]]
          [(str "    " (paint codes glyph) " " (paint codes (str (count rows)))
                " " (paint :bold (str verb " " (plural (count rows) cls))))
           (when (seq names) (grid names 8 codes))
           (for [r rows :when (:seq r)]
             (str "        "
                  (paint :dim (str (i18n/t :cli/seq) " " (:seq r) "  " (or (row-name r) "")))))
           (when verbose?
             (for [r rows :when (seq (:changes r))]
               (str "        " (paint :dim "·") " " (:type r) " " (:short-id r) ": "
                    (str/join ", " (map #(name (:attr %)) (:changes r))))))]))
      (when (pos? n) (str "    " (paint :dim (i18n/t :cli/changeset-drop-hint))))])))

(defmethod step! :review [ctx [_ {:keys [title verbose?]}]]
  (let [staged (:staged (req! "list staged" (api/get! :staging)))]
    (run! (partial say! ctx) (review-lines staged title verbose?))
    (when-not (prompt-yes? ctx (i18n/t :cli/apply-these))
      (say! ctx (mark :red "✗" (paint :dim (i18n/t :cli/declined-left-staged))))
      :abort)))

(defmethod step! :confirm [ctx [_ {:keys [message]}]]
  (when-not (prompt-yes? ctx (or message (i18n/t :cli/proceed)))
    (say! ctx (mark :red "✗" (i18n/t :cli/declined)))
    :abort))

(defmethod step! :show [ctx [_ {:keys [title objects]}]]
  (let [page (when objects (req! (str "list " objects) (api/list-objects objects)))
        rows (cond (sequential? page) page
                   (:rows page)       (:rows page)
                   :else              (:objects page))]
    (emit! ctx (cond-> {}
                 title                (assoc :title title)
                 objects              (assoc :objects (mapv :values rows))
                 (:next-offset page)  (assoc :total (:total page)
                                             :next-offset (:next-offset page))))))

(defn- table-lines [cols rows]
  (let [width (fn [[h f]] (apply max (count h) (map #(count (f %)) rows)))
        widths (mapv width cols)
        pad  (fn [w s] (format (str "%-" w "s") s))
        line (fn [vals] (str/trimr (str/join "  " (map pad widths vals))))]
    (cons (str "    " (paint :bold (line (map first cols))))
          (for [row rows] (str "    " (line (map (fn [[_ f]] (f row)) cols)))))))

(defn- unknown-show-cols
  [show rows]
  (when (and (seq show) (seq rows))
    (let [present (into #{} (mapcat (comp keys :values)) rows)
          wanted  (remove map? show)]
      (->> wanted (remove present) (map name) sort vec seq))))

(defn- query-shape
  [find]
  (let [{:keys [data]} (req! "describe" (api/get! :schema-describe {:names (name find)
                                                                     :no-count true}))]
    {::described? true
     :object-types
     (if-not (:object-type data)
       {}
       {(:rid data) {:object-type-name (:object-type data)
                     :title-key        (some-> (:title data) keyword)
                     :primary-key      (:primary-key data)
                     :attribute-list   (mapv (fn [{:keys [name type]}]
                                               {:attribute-name name :attribute-type type})
                                             (:fields data))}})}))

(defmethod step! :query
  [ctx [_ {:keys [title show count-only? explain? save? freeze? latest find] :as spec0}]]
  (let [want-order? (boolean (and latest find (not (:order-by spec0))))
        want-cols?  (boolean (and find (empty? show)
                                  (not count-only?) (not explain?) (not save?)))
        body        (when (or want-order? want-cols?) (query-shape find))
        spec        (cond-> (dissoc spec0 :latest)
                      want-order? (merge (latest-order body find)))
        cols        (or (not-empty show)
                        (when want-cols? (default-show-cols body find (:order-by spec))))]
    (cond
      save?
      (let [payload (-> (dissoc spec :title :count-only? :count? :show
                                :save? :freeze? :explain? :engine :accuracy)
                        (cond-> freeze? (assoc :materialize true)))
            s (req! "query --save" (api/post! :object-sets-temp payload))]
        (emit! ctx (dissoc s :ir)))

      explain?
      (emit! ctx (req! "query --explain"
                       (api/post! :ontology-query (dissoc spec :title :count-only?))))

      :else
      (let [{:keys [rows count]} (req! "query"
                                       (api/post! :ontology-query
                                                  (dissoc spec :title :count-only?)))
            n (or count (clojure.core/count rows))]
        (cond
          count-only?
          (emit! ctx {:count n})
          :else
          (do (emit! ctx (cond-> {:rows (project-rows show rows) :count n :columns (vec cols)}
                           title (assoc :title title))
                     ::query)
              (when-let [hint (and body (empty? show) (title-key-hint body find))]
                (say! ctx hint))))
        (doseq [[[a _] g] (group-by (juxt :attribute :over)
                                    (for [r rows, [a vs] (:defects r), v vs]
                                      (assoc v :attribute a)))]
          (warn! (mark :yellow "⚠" (str (name a) ": " (:why (first g))
                                        " (" (clojure.core/count g) " Zeile(n))"))))
        (when-let [bad (unknown-show-cols show rows)]
          (warn! (mark :yellow "⚠"
                       (str "--show " (str/join "," bad) " produced no values — "
                            "unknown field(s), or null across all " (clojure.core/count rows)
                            " row(s); check `describe " (some-> (:find spec) name) "`")))))))
  nil)

(defmethod render ::query [_ {:keys [title rows count columns]}]
  [(when title (paint :bold (str "  ── " title " ──")))
   (table-lines (query-columns columns) rows)
   (str "    " (paint :dim (str count " " (i18n/t :cli/results))))])

(defmethod step! :series [ctx [_ spec]]
  (let [{:keys [property unit bucket rollup sync series points objects]}
        (req! "series" (api/post! :ontology-series spec))]
    (emit! ctx {:property property :unit unit :bucket bucket :rollup rollup
                :sync sync :series series :points points :objects objects})))

(defmethod step! :object-sets [ctx [_ {:keys [delete]}]]
  (if delete
    (let [{:keys [deleted]} (req! "object-sets --delete"
                                  (api/delete! :object-set-temp-by {:rid delete}))]
      (emit! ctx {:deleted (boolean deleted)}))
    (let [{:keys [object-sets temporary]} (req! "object-sets" (api/get! :object-sets))]
      (emit! ctx {:object-sets object-sets :temporary temporary}))))

(defn- object-primary-key
  [body nm]
  (if-let [[_ spec] (and (::described? body) (object-type-entry (:object-types body) nm))]
    (:primary-key spec)
    (get-in (backing*) [:object-types (clojure.core/name nm) :primary-key])))

(defn- default-show-cols
  [body nm order-key]
  (when-let [[_ spec] (object-type-entry (:object-types body) nm)]
    (let [head (or (some-> (:title-key spec) keyword)
                   (some-> (object-primary-key body nm) keyword))]
      (vec (distinct (remove nil? [head order-key]))))))

(defn- title-key-hint
  [body nm]
  (when-let [[_ spec] (object-type-entry (:object-types body) nm)]
    (when-not (:title-key spec)
      (str "    " (paint :dim (i18n/t :cli/no-title-declared {:typ (clojure.core/name nm)}))))))

(defmethod step! :describe [ctx [_ {:keys [sections section project]}]]
  (let [toks (cond (seq sections) (vec sections) section [section] :else [])
        r    (api/get! :schema-describe (cond-> {}
                                          (seq toks) (assoc :names (str/join "," (map name toks)))
                                          project    (assoc :project (name project))))
        {:keys [shape data no-project]} (req! "describe" r)]
    (when no-project
      (throw (ex-info (str "no project named " (pr-str no-project)
                           " — `project list` names them")
                      {:project no-project})))
    (emit! ctx data (case (keyword shape)
                      :overview :default
                      :one      ::described-one
                      :many     ::described))))

(defn- described-lines [j]
  [(show-lines j) (when (map? j) (:code j))])

(defmethod render ::described-one [_ j] (described-lines j))

(defmethod render ::described [_ {:keys [described]}]
  (map described-lines (vals described)))
