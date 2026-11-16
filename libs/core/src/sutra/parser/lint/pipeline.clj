(ns sutra.parser.lint.pipeline
  (:require [sutra.parser.lint.support :refer [->name log-generator ot-name code-attr-re outs-of
                                               ins-of action-managed-types producers-of]]
            [clojure.string :as str]
            [sutra.types.backing :as backing]
            [sutra.types.forms :as forms]
            [sutra.types.membership :as member]
            [sutra.types.languages :as langs]))

(set! *warn-on-reflection* true)

(def rules
  [{:id :denormalized-link-property :severity :info
    :doc nil
    :scan (fn [schema]
            (let [obj-types (:object-types schema)
                  rid->name (into {} (for [[rid spec] obj-types]
                                       [rid (some-> (:object-type-name spec) ->name)]))
                  nbr-names (reduce
                             (fn [m [_rid l]]
                               (let [f  (:from-object-type l), t (:to-object-type l)
                                     fn* (rid->name f), tn (rid->name t)]
                                 (cond-> m
                                   (and f tn) (update f (fnil conj #{}) tn)
                                   (and t fn*) (update t (fnil conj #{}) fn*))))
                             {}
                             (:link-types schema))]
              (for [[rid spec] obj-types
                    :let [an-name (rid->name rid)
                          nbrs    (get nbr-names rid)]
                    :when (seq nbrs)
                    a (:attribute-list spec)
                    :let [attr (->name (:attribute-name a))
                          low  (some-> attr str/lower-case)]
                    :when (and attr
                               (not (get-in a [:opts :derived]))
                               (not (re-find code-attr-re attr))
                               (some (fn [nbr]
                                       (let [n (str/lower-case nbr)]
                                         (and (not= n low)
                                              (or (str/starts-with? low (str n "-"))
                                                  (str/starts-with? low (str n "_"))))))
                                     nbrs))]
                {:entity (str an-name "." attr) :entity-class :object-types
                 :message (str "Attribute " (pr-str attr) " on " (pr-str an-name)
                               " is named after a linked object type — it looks like a stored"
                               " COPY of a linked object's non-key field, which goes stale when"
                               " the source object changes. Don't copy a linked property into a"
                               " column: select it live across the link (a :derived property, or"
                               " `query --where <link>.<attr>=…`). Store only the foreign key,"
                               " never the neighbour's non-key fields.")})))}

   {:id :link-fk-backing :severity :error
    :doc nil
    :scan (fn [schema]
            (let [ltypes  (:link-types schema)
                  ln-name (fn [rid] (or (some-> (get ltypes rid) :link-type-name ->name)
                                        (->name rid)))]
              (for [[_ t] (:transforms schema)
                    o (outs-of t)
                    :when (= :link-type (:kind o))
                    :let [rid  (:link-type o)
                          nm   (ln-name rid)
                          lt   (get ltypes rid)
                          card (:cardinality lt)
                          lang (:language t)
                          strat (:strategy t)
                          reason
                          (cond
                            (= :resolve-by-shared-property strat)
                            (str "Link-type " (pr-str nm)
                                 " is backed by :resolve-by-shared-property, which resolves the"
                                 " target on a non-primary-key shared property. the target side"
                                 " always resolves on its PRIMARY KEY. Compute the foreign key"
                                 " upstream in the source pipeline and declare the link `{:fk …}`,"
                                 " or use `{:fk-resolve …}` sugar.")
                            (backing/endpoint-fk-output-problem o)
                            (str "Link-type " (pr-str nm) " is backed by "
                                 (backing/endpoint-fk-output-problem o))
                            (and lt (langs/compute-language? lang)
                                 (not= :polymorphic strat)
                                 (not= :many-to-many card))
                            (str "Link-type " (pr-str nm) " is "
                                 (pr-str card) " but is backed by a " (name lang)
                                 " transform that materialises an edge-pair join table"
                                 " (:from-key-column/:to-key-column). every other cardinality is"
                                 " an object-type foreign key on the many side. Move the match"
                                 " into an FK attribute and declare it `{:fk …}`, or — if the edge"
                                 " really is many-to-many — set :cardinality :many-to-many."))]
                    :when reason]
                {:entity nm :entity-class :link-types :message reason})))}

   {:id :source-syncs-into-ontology :severity :error :gate? true
    :doc nil
    :scan (fn [schema]
            (for [[_ t] (:transforms schema)
                  :when (some #(= :datasource (:kind %)) (ins-of t))
                  o (outs-of t)
                  :when (and (= :object-type (:kind o))
                             (not= :supplement (:role o)))
                  :let [nm (->name (:transform-name t))]]
              {:entity nm :entity-class :transforms
               :message (str "Transform " (pr-str nm) " syncs a DATASOURCE directly into"
                             " object-type " (pr-str (ot-name schema (:object-type o)))
                             " — raw source rows land in the ontology with no curated layer"
                             " between, so every source column becomes a property and there is"
                             " nowhere to cast, rename or drop ETL columns. Bind"
                             " `:external-source` to a `:datasets` entry instead (it lifts to a"
                             " `<name>-sync` transform), then shape that dataset into the"
                             " object type with a second transform.")}))}

   {:id :ontology-as-pipeline-intermediate :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[_ t] (:transforms schema)
                  :let [ins  (filter #(= :object-type (:kind %)) (ins-of t))
                        outs (filter #(= :object-type (:kind %)) (outs-of t))]
                  :when (and (seq ins) (seq outs))
                  o outs
                  :when (not= :supplement (:role o))
                  :let [nm (->name (:transform-name t))]]
              {:entity nm :entity-class :transforms
               :message (str "Transform " (pr-str nm) " READS object-type(s) "
                             (pr-str (mapv #(ot-name schema (:object-type %)) ins))
                             " and WRITES object-type " (pr-str (ot-name schema (:object-type o)))
                             " — that is dataset-layer work (aggregation, dedup, resolution)"
                             " hosted in the ontology. The ontology is not the pipeline's"
                             " scratch space: end users see the intermediates, and"
                             " `:semantic-version` auto-truncate does NOT fire for an"
                             " :object-type output, so the step cannot be cleanly"
                             " reprocessed. Terminate it in a `:dataset` output and back the"
                             " object type off that with a shaping transform.")}))}

   {:id :transform-writes-own-input :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[_ t] (:transforms schema)
                  :let [in-rids (into #{} (keep #(when (= :object-type (:kind %))
                                                   (:object-type %)))
                                      (ins-of t))]
                  o (outs-of t)
                  :when (and (= :object-type (:kind o))
                             (contains? in-rids (:object-type o))
                             (not= :supplement (:role o)))
                  :let [nm (->name (:transform-name t))]]
              {:entity nm :entity-class :transforms
               :message (str "Transform " (pr-str nm) " reads object-type "
                             (pr-str (ot-name schema (:object-type o)))
                             " and writes back to the SAME type — the pipeline mutating the"
                             " ontology in place, typically to stamp a resolved-* foreign key"
                             " onto the object. A relationship belongs in a LINK, not a column:"
                             " compute the resolution into a `:dataset` and declare an"
                             " `:endpoint-fk` link over it. The FK column then leaves the"
                             " object type entirely.")}))}

   {:id :materialization-without-edits :severity :info
    :doc nil
    :scan (fn [schema]
            (let [edited (action-managed-types schema)]
              (for [[_ ds] (:datasets schema)
                    :when (= :materialization (:kind ds))
                    :let [ot (ot-name schema (:object-type ds))]
                    :when (not (contains? edited ot))]
                {:entity (->name (:dataset-name ds)) :entity-class :datasets
                 :message (str "Materialization " (pr-str (->name (:dataset-name ds)))
                               " of " (pr-str ot) ": no action edits that type. A"
                               " materialization is for a pipeline that needs the state with"
                               " user edits, or for a download; a pipeline otherwise reads the"
                               " output dataset of " (pr-str (vec (producers-of schema
                                                                                (:object-type ds))))
                               ". It stays right where a derived property is read.")})))}

   {:id :transform-reads-object-table :severity :warn
    :doc nil
    :scan (fn [schema]
            (let [table? (into #{}
                               (mapcat (fn [[k ds]]
                                         (when (= :object-backing (keyword (:kind ds)))
                                           [k (->name (:dataset-name ds))])))
                               (:datasets schema))]
              (for [[_ t] (:transforms schema)
                    in (ins-of t)
                    :when (and (= :dataset (:kind in))
                               (or (table? (:dataset in)) (table? (->name (:dataset in)))))]
                {:entity (->name (:transform-name t)) :entity-class :transforms
                 :message (str "Transform " (pr-str (->name (:transform-name t))) " reads "
                               (pr-str (->name (:dataset in))) ", a type's object table. No"
                               " build orders it after what produces the type, so it reads an"
                               " older state. Read the producer's output dataset, or the"
                               " type's materialization where its edits matter.")})))}

   {:id :etl-column-as-property :severity :info
    :doc nil
    :scan (fn [schema]
            (let [pk-of (reduce (fn [m [_ t]]
                                  (reduce (fn [m o]
                                            (if-let [pk (and (= :object-type (:kind o))
                                                             (or (:primary-key o)
                                                                 (:primary-key t)))]
                                              (update m (:object-type o) (fnil conj #{})
                                                      (->name pk))
                                              m))
                                          m (outs-of t)))
                                {} (:transforms schema))
                  fk-backed (reduce
                             (fn [m [_ t]]
                               (reduce (fn [m o]
                                         (if-let [a (and (= :link-type (:kind o))
                                                         (:fk-attribute o))]
                                           (if-let [from (:from-object-type
                                                          (get (:link-types schema)
                                                               (:link-type o)))]
                                             (update m from (fnil conj #{}) (->name a))
                                             m)
                                           m))
                                       m (outs-of t)))
                             {} (:transforms schema))]
              (for [[rid spec] (:object-types schema)
                    :let [nm (or (some-> (:object-type-name spec) ->name) (->name rid))]
                    :when nm
                    a (:attribute-list spec)
                    :let [attr (->name (:attribute-name a))
                          low  (some-> attr str/lower-case)]
                    :when (and low
                               (re-find #"(?i)^(resolved[_-]|import[_-]id$)" low)
                               (not (contains? (get pk-of rid #{}) attr))
                               (not (contains? (get fk-backed rid #{}) attr))
                               (not= :hidden (get-in a [:opts :visibility])))]
                {:entity (str nm "." attr) :entity-class :object-types
                 :message (str "Attribute " (pr-str attr) " on " (pr-str nm)
                               " is pipeline scratch exposed as an ontology property — a"
                               " resolution result or an ETL batch id. If it carries a"
                               " relationship, model that as a link type instead.")})))}

   {:id :action-sprawl :severity :info
    :doc nil
    :scan (fn [schema]
            (let [targets  (fn [a] (distinct (keep :object-type (:rules a))))
                  one-prop? (fn [a] (let [rs (:rules a)]
                                      (and (= 1 (count rs))
                                           (= 1 (count (:values (first rs)))))))
                  by-type  (reduce (fn [m [nm a]]
                                     (reduce (fn [m t] (update m t (fnil conj []) [nm a]))
                                             m (targets a)))
                                   {} (:actions schema))]
              (for [[t acts] (sort-by (comp str key) by-type)
                    :let [n     (count acts)
                          singles (filterv (comp one-prop? second) acts)]
                    :when (> n 10)]
                {:entity (->name t) :entity-class :object-types
                 :message (str "Object type " (pr-str (->name t)) " has " n " action types."
                               (when (seq singles)
                                 (str " " (count singles) " of them edit exactly ONE property ("
                                      (str/join ", " (sort (map (comp ->name first) singles)))
                                      "), which is the shape the anti-pattern names: actions"
                                      " thought of as database column updates rather than"
                                      " business operations."))
                               " Bundle the related changes into actions that represent a"
                               " meaningful business operation, so one submission completes one"
                               " decision instead of N.")})))}

   {:id :resource-without-project :severity :error :gate? true :scope :change
    :doc "every resource of an owned kind has a place, a project — its :project, or the one the ownership table derives (sutra.types.membership). A gate, so no path writes one without; one a world already carries stands until it is fixed."
    :scan (fn [schema]
            (let [lens (forms/installed schema)]
              (for [[tag entry] (sort-by (comp str first) (member/without-project lens))
                    :let [nm  ((:name-of lens) tag entry)
                          gen (when (= :object-type tag) (log-generator schema nm))]]
                {:entity nm :entity-class tag
                 :message (str (name tag) " " (pr-str nm)
                               " belongs to no project. Every resource has a place"
                               ": the text it sits in, the project whose owners "
                               "judge a"
                               " breaking change to it, and a node `project deps` can see. "
                               "Set :project —"
                               " or write it into a text of that project. Using it from another "
                               "project needs"
                               " nothing."
                               (when gen
                                 (str " Nobody wrote this one: it is the submission log of action "
                                      (pr-str gen)
                                      ", generated from its :action-log, and a log is placed where"
                                      " its action is. So place the ACTION — "
                                      "`:project` on "
                                      (pr-str gen)
                                      ", or write it into a project's text — and the log follows;"
                                      " naming the log would place the generated node instead of "
                                      "its cause.")))})))}])
