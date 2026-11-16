(ns sutra.parser.lint.backing
  (:require [sutra.types.kinds :as kinds]
            [sutra.parser.lint.support :refer [->name section-entries ot-name outs-of ins-of
                                               type-ref action-log-types]]
            [clojure.string :as str]
            [sutra.types.submission :as csub]
            [sutra.types.timeseries :as ts]))

(set! *warn-on-reflection* true)

(def rules
  [{:id :backing-without-producer :severity :warn
    :doc nil
    :scan (fn [schema]
            (let [ds-name  (fn [d] (or (some-> (get-in schema [:datasets d :dataset-name]) ->name)
                                       (->name d)))
                  filled   (into (into #{}
                                       (comp (mapcat (fn [[_ t]] (outs-of t)))
                                             (keep (fn [o] (when (= :dataset (:kind o))
                                                             (some-> (:dataset o) ds-name)))))
                                       (:transforms schema))
                                 (for [[nm spec] (section-entries (:datasets schema) :dataset-name)
                                       :when (or (:external-source spec) (:stream spec)
                                                 (#{:standalone
                                                    :virtual} (some-> (:kind spec) keyword)))]
                                   nm))]
              (for [[nm spec] (section-entries (:object-types schema) :object-type-name)
                    :let [ds (some-> (get-in spec [:backing :dataset]) ds-name)]
                    :when (and ds (not (filled ds)))]
                {:entity nm :entity-class :object-types
                 :message (str "Object type " (pr-str nm) " declares :backing " (pr-str ds)
                               " but nothing fills that dataset — no transform outputs it,"
                               " it declares no :external-source and it is no standalone"
                               " (upload) or virtual dataset — so the type holds no objects. Give "
                               "the"
                               " dataset a producer, or drop the :backing.")})))}

   {:id :names-undeclared-dataset :severity :warn
    :doc nil
    :scan (fn [schema]
            (let [names    (fn [x] (when x (if (keyword? x) (name x) (str x))))
                  known    (into #{} (mapcat (fn [[k v]] [(names k) (names (:dataset-name v))]))
                                 (:datasets schema))
                  missing? #(and % (not (known (names %))))]
              (concat
               (for [[_ t] (:transforms schema)
                     io    (concat (ins-of t) (outs-of t))
                     :when (and (= :dataset-table (kinds/physical (:kind io)))
                                (missing? (:dataset io)))
                     :let  [nm (->name (:transform-name t))]]
                 {:entity nm :entity-class :transforms
                  :message (str "Transform " (pr-str nm) " names dataset "
                                (pr-str (names (:dataset io)))
                                ", which is declared nowhere — every run of it fails, and every"
                                " build that includes it. Declare the dataset or retire the"
                                " transform.")})
               (for [[_ a] (:applications schema)
                     :let  [ds (get-in a [:layout :upload :dataset])
                            nm (->name (:application-name a))]
                     :when (missing? ds)]
                 {:entity nm :entity-class :applications
                  :message (str "App " (pr-str nm) " uploads into dataset " (pr-str (names ds))
                                ", which is declared nowhere — its upload cannot land. Declare"
                                " the dataset (:kind :standalone) or drop :layout :upload.")}))))}

   {:id :writeback-without-read-side :severity :warn
    :doc nil
    :scan (fn [schema]
            (let [produced (into #{}
                                 (comp (mapcat (fn [[_ t]] (outs-of t)))
                                       (keep (fn [o] (some->> (:object-type o)
                                                              (ot-name schema)))))
                                 (:transforms schema))
                  backed   (into #{} (for [[nm spec] (section-entries (:object-types schema)
                                                                      :object-type-name)
                                           :when (:backing spec)]
                                       nm))
                  written  (into {} (for [[_ s] (:sources schema)
                                          [table allow] (:writeback s)
                                          :let [ot (some-> (:object-type allow) ->name)]
                                          :when ot]
                                      [ot table]))]
              (for [[ot table] (sort-by key written)
                    :when (not (or (produced ot) (backed ot)))]
                {:entity ot :entity-class :object-types
                 :message (str "Object type " (pr-str ot) " is writable through the"
                               " :writeback allowlist of " (pr-str table)
                               " but no transform outputs it, so it only ever holds"
                               " what actions wrote — never the rows " (pr-str table)
                               " already has, and an :object-type parameter finds"
                               " those. Add the transform that reads "
                               (pr-str table) ".")})))}

   {:id :criterion-without-message :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[nm a] (sort-by (comp str key) (:actions schema))
                  :let [mute (remove #(seq (str (:message %))) (:submission-criteria a))]
                  :when (seq mute)]
              {:entity (->name nm) :entity-class :actions
               :message (str "Action " (pr-str (->name nm)) " has " (count mute)
                             " submission criterion/criteria without a :message. authored"
                             " ONCE at the action, shown by every consumer. Without it the"
                             " refusal reaching the user is generic, and only the author"
                             " knows why the rule exists.")}))}

   {:id :create-without-key-param :severity :info
    :doc nil
    :scan (fn [schema]
            (for [logs   [(action-log-types schema)]
                  [nm a] (sort-by first (section-entries (:actions schema) :action-name))
                  [i r] (map-indexed vector (:rules a))
                  :let [id (:id r)]
                  :when (and (= :create (keyword (:op r)))
                             (not (contains? logs (type-ref schema (:object-type r))))
                             (not (str/starts-with?
                                   (str
                                    (:plural (or (get-in schema [:object-types (:object-type r)])
                                                 (some (fn [[_ o]]
                                                         (when (= (type-ref schema (:object-type r))
                                                                  (some-> (:object-type-name o)
                                                                          ->name))
                                                           o))
                                                       (:object-types schema)))))
                                   "[LOG]")))
                  :when (and (= :create (keyword (:op r)))
                             (or (nil? id)
                                 (and (vector? id) (= 2 (count id)) (= :get (keyword (first id)))
                                      (csub/implicit-create-id? (keyword (second id))))))]
              {:entity nm :entity-class :actions
               :message (str "Regel " i " von " (pr-str nm) " legt "
                             (pr-str (->name (:object-type r)))
                             " an, ohne dass ein Parameter den Schluessel nennt"
                             (if id
                               (str " — die Plattform setzt dafuer den verborgenen Parameter "
                                    (pr-str (keyword (second id))) " ein.")
                               " — jeder Aufruf zieht eine neue UUID, Vorschau und Ausfuehrung zeigen verschiedene.")
                             " Ein angelegtes Objekt bekommt Schluessel und Eigenschaften allein"
                             " aus Parametern. Setze :id der Regel auf einen Parameter mit "
                             ":prefill :uuid — am"
                             " besten den, der schon den Primaerschluessel fuellt.")}))}

   {:id :timeseries-property-declares-sync :severity :error
    :doc nil
    :scan (fn [schema]
            (let [ds-cols (into {} (map (fn [[_ d]] [(->name (:dataset-name d)) (:columns d)]))
                                (:datasets schema))
                  ds?     (set (keys ds-cols))]
              (for [[_ spec] (:object-types schema)
                    :let [nm (some-> (:object-type-name spec) ->name)]
                    :when nm
                    a (:attribute-list spec)
                    :let [attr   (->name (:attribute-name a))
                          series (get-in a [:opts :series])
                          tsp?   (= :timeseries (:attribute-type a))
                          sync   (:sync series)
                          sync-n (->name sync)
                          entity (str nm "." attr)
                          finding #(hash-map :entity entity :entity-class :object-types
                                             :message %)]
                    :when attr
                    f (cond
                        (and tsp? (nil? sync-n))
                        [(finding (str "Property " (pr-str attr) " on " (pr-str nm)
                                       " is a :timeseries but names no sync — add"
                                       " `:series {:sync <dataset> :rollup :sum}`. Its column"
                                       " holds a SERIES ID, and without a sync nothing"
                                       " resolves it."))]

                        (and (not tsp?) (some? series))
                        [(finding (str "Property " (pr-str attr) " on " (pr-str nm)
                                       " carries a :series binding but is typed "
                                       (pr-str (:attribute-type a))
                                       " — only a :timeseries property is read as a series;"
                                       " the binding is inert here."))]

                        (and tsp? (not (ds? sync-n)))
                        [(finding (str "Property " (pr-str attr) " on " (pr-str nm)
                                       " names sync " (pr-str sync)
                                       ", which is not a dataset in this ontology."))]

                        tsp?
                        (when-let [p (ts/explain-sync sync (get ds-cols sync-n))]
                          [(finding (str "Property " (pr-str attr) " on " (pr-str nm)
                                         " cannot be answered: " (:message p)))])

                        :else nil)]
                f)))}])
