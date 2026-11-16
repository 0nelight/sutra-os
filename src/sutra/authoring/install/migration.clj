(ns sutra.authoring.install.migration
  (:require [sutra.types.value-types :as vt]
            [sutra.catalog.backing :as backing]
            [sutra.catalog.retirement :as retirement]
            [sutra.catalog.transforms :as transforms]
            [sutra.types.world-rules :as rules]
            [sutra.catalog.shape :as shape]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.live :as live]
            [sutra.authoring.install.findings :as fnd]
            [sutra.kit.coerce :refer [drop-empty ->name-kw]]
            [sutra.types.attribute :as attribute]
            [sutra.types.schema :as sch]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(defn- attrs-by-name
  [ot]
  (:types (attribute/of ot)))

(defn- types-by-name
  [schema]
  (into {} (map (juxt (comp :object-type-name val) val)) (:object-types schema)))

(defn- struct-fields
  [t]
  (when (and (vector? t) (= :struct (first t)) (map? (second t)))
    (second t)))

(defn- ontology-type
  [t]
  (if (and (vector? t) (= :decimal (first t))) :decimal t))

(defn- backing-ref
  [schema ot-name]
  (when-let [{:keys [authored? dataset producer]} (backing/object-backing schema ot-name)]
    (if authored?
      {:dataset dataset}
      (let [t (get-in schema [:transforms producer])]
        {:producer   producer
         :datasource (or (get-in t [:input :datasource])
                         (get-in t [:from :datasource]))}))))

(defn- backing-token
  [{:keys [producer dataset]}]
  (->name-kw (or producer dataset)))

(defn- type-changes
  [ot-name la da edited]
  (mapcat
   (fn [[a t]]
     (let [t' (get da a)]
       (when (and t' (not= (ontology-type t) (ontology-type t')))
         (let [lf (struct-fields t) df (struct-fields t')]
           (if (and lf df)
             (concat
              (for [[f ft] lf :let [ft' (get df f)]
                    :when (and ft' (not= ft ft'))]
                {:object-type ot-name :change :struct-field-type-changed
                 :property a :field f :from ft :to ft'})
              (for [[f ft] lf
                    :when (and (not (contains? df f))
                               (contains? (force edited) [ot-name a]))]
                {:object-type ot-name :change :struct-field-deleted
                 :property a :field f :from ft}))
             (when-not (vt/same-storage? t t')
               [{:object-type ot-name :change :property-type-changed
                 :property a :from t :to t'}]))))))
   la))

(defn breaking-schema-changes
  [desired live edited]
  (let [desired (sch/indexed desired)
        live    (sch/indexed live)
        d (types-by-name desired)
        l (types-by-name live)]
    (vec
     (mapcat
      (fn [[ot-name live-ot]]
        (when-let [want (get d ot-name)]
          (let [la  (attrs-by-name live-ot)
                da  (attrs-by-name want)
                lpk (rules/object-primary-key live ot-name)
                dpk (rules/object-primary-key desired ot-name)
                lb  (backing-ref live ot-name)
                db  (backing-ref desired ot-name)
                renamed (shape/property-renames la da)
                gone (into {} (remove (fn [[a _]] (or (contains? da a) (contains? renamed a)))) la)
                added (into {} (remove (fn [[a _]] (or (contains? la a)
                                                       (some #{a} (vals renamed)))))
                            da)]
            (concat
             (let [lr (backing/identity-rule live-ot lpk)
                   dr (backing/identity-rule want dpk)]
               (when (and (backing/identity-rule-determinate? lr)
                          (backing/identity-rule-determinate? dr)
                          (not= lr dr))
                 [{:object-type ot-name :change :primary-key-changed :from lr :to dr}]))
             (when (and lb db (not= lb db))
               [{:object-type ot-name :change :backing-changed :from lb :to db
                 :confirmed? (= (backing-token lb)
                                (->name-kw (get-in want [:backing :replaces])))}])
             (type-changes ot-name la da edited)
             (for [[a a'] renamed]
               {:object-type ot-name :change :property-renamed :from a :to a'})
             (mapcat
              (fn [[a t]]
                (when (contains? (force edited) [ot-name a])
                  (cons {:object-type ot-name :change :property-deleted :property a :from t}
                        (for [[a' t'] added :when (= t t')]
                          {:object-type ot-name :change :property-renamed?
                           :property a :candidate a' :type t}))))
              gone)))))
      l))))

(defn- minted-key
  [[_ spec o]] (or (:primary-key o) (:primary-key spec)))

(defn- mint-rule
  [obj-spec producer]
  (backing/identity-rule obj-spec (some-> producer minted-key)))

(defn- answerable-types
  [{:keys [plan after]}]
  (let [touched (fn [kind] (into #{} (comp (filter #(and (= kind (:kind %))
                                                         (not= :skipped (:outcome %))))
                                           (map :entity))
                                 plan))]
    (into (touched :object-type)
          (for [t (touched :transform)
                o (tio/outputs-of (live/lookup (:transforms after) t))
                :when (and (= :object-type (:kind o)) (not= :supplement (:role o)))]
            (:object-type o)))))

(defn- identity-rule-of
  [{{live-objs :object-types live-tx :transforms} :before
    {after-ot :object-types} :after :as d} nm]
  (let [after     (live/lookup after-ot nm)
        live-spec (live/lookup live-objs nm)
        prod      (diff/primary-output d :after :object-type nm)
        [t-name t-spec] prod
        pk        (some-> prod minted-key)
        mks       (vec (:match-keys after))
        was       (mint-rule live-spec (diff/primary-output d :before :object-type nm))
        now       (mint-rule after prod)
        known?    (fn [[kind v]] (if (= :entity-resolution kind) (seq v) (some? v)))
        moved?    (and after live-spec (known? was) (known? now) (not= was now))
        live-t    (live/lookup live-tx t-name)
        full-run? (or (nil? live-t)
                      (not= (:semantic-version t-spec) (:semantic-version live-t)))
        ctx       {:match-keys mks :primary-key pk :producer t-name
                   :status (:status live-spec) :was was :now now}]
    (cond
      (and after (not (:resolve-entity after))
           pk (> (count mks) 1) (contains? (set mks) pk))
      {:rule :identity-inert :severity :error ::fnd/ctx ctx}

      (and moved? (contains? retirement/protected-statuses (:status live-spec)))
      {:rule :identity-locked :severity :error ::fnd/ctx ctx}

      (and moved? (not (and t-spec
                            (transforms/snapshot-write? t-spec)
                            (or (not (:incremental t-spec)) full-run?))))
      {:rule :identity-unretractable :severity :error ::fnd/ctx ctx}

      moved?
      {:rule :identity-remint :severity :warn ::fnd/ctx ctx})))

(defn- move-str [{:keys [was now]}] (str (pr-str was) " → " (pr-str now)))

(defn- say-inert
  [{nm :entity {:keys [match-keys primary-key producer]} ::fnd/ctx}]
  (str "Object type " (pr-str nm) " declares :match-keys " (pr-str match-keys)
       ", but its ids are minted from the :primary-key "
       (pr-str primary-key) " of " (pr-str producer)
       " — one of those very keys."
       " The declaration promises a grain the platform does not key by:"
       " every row sharing " (pr-str primary-key) " collapses onto ONE id, last"
       " write winning, and the rows under the other declared key(s) are"
       " silently absent — no build, no check and no push reports it."
       " On a non-resolved type :match-keys derives NOTHING; the"
       " producer's :primary-key is the key. Fix one of the three:"
       " give the producer a :primary-key column carrying the whole"
       " declared grain (compute it in the shaping transform), or mark"
       " the type :resolve-entity true so entity resolution keys by"
       " :match-keys, or drop the extra key(s) if " (pr-str primary-key)
       " really is the grain."))

(defn- say-locked
  [{nm :entity {:keys [status] :as ctx} ::fnd/ctx}]
  (str "Object type " (pr-str nm) " is " status
       " and this install changes how its ids are minted ("
       (move-str ctx) "). Every id the type holds would change, every edit attached to an"
       " old id is stranded, and every consumer holding one breaks. Demote"
       " it to :experimental or :deprecated in its own push first — the"
       " same lifecycle gate a delete or a rename goes through — and then"
       " move the key."))

(defn- say-unretractable
  [{nm :entity {:keys [producer] :as ctx} ::fnd/ctx}]
  (str "This install changes how " (pr-str nm) "'s ids are minted ("
       (move-str ctx) "), and " (if producer
                                  (str "its producer " (pr-str producer)
                                       " would not retract the rows minted under"
                                       " the old rule")
                                  "the type has no producer to re-mint them")
       ". A new key-minting rule does not overwrite the old rows — it"
       " mints NEW ones beside them, and on an append-only output nothing"
       " ever retracts the strays: a consumer grouping on a value then"
       " sees BOTH. Ship it as the pair the ontology README §7 states —"
       " bump the producer's :semantic-version (forces one full,"
       " non-incremental run) AND set :write-mode :replace (makes that run"
       " authoritative, so the deletion reconcile retires what it omits)."
       " Guard the empty case: an empty authority under a replace write"
       " deletes everything — raise instead."))

(defn- say-remint
  [{nm :entity ctx ::fnd/ctx}]
  (str "Object type " (pr-str nm) "'s ids will be RE-MINTED ("
       (move-str ctx) "). Every object gets a new id at the next build. (1) The backing is"
       " re-registered and the objects are \"unavailable in user"
       " applications during that reindex time\". (2) Every user EDIT is"
       " attached to the primary-key value it was made for: those values no"
       " longer occur, so the edits are stranded — and they EXEMPT their"
       " old rows from the deletion sweep, which keeps precisely those"
       " strays alive. the equivalent is `sutra object drop-edits " (name nm)
       "`, after the build. Then hold the two halves against each other:"
       " `describe " (name nm) " --fields match-keys,primary-key,count`."))

(def identity-changes
  {:find (fn [d]
           (for [nm (sort (answerable-types d))
                 :let [f (identity-rule-of d nm)]
                 :when f]
             (assoc f :entity nm)))
   :says {:identity-inert         say-inert
          :identity-locked        say-locked
          :identity-unretractable say-unretractable
          :identity-remint        say-remint}})

(defn- stranding-findings
  [after live {edited :properties :keys [types rows]}]
  (let [brks   (breaking-schema-changes after live edited)
        cands (reduce (fn [m {:keys [object-type property candidate change]}]
                        (cond-> m
                          (= :property-renamed? change)
                          (update [object-type property] (fnil conj []) candidate)))
                      {} brks)
        n-rows (fn [ot] (get (force rows) (->name-kw ot) 0))]
    (vec
     (concat
      (for [{:keys [object-type confirmed?] :as b} brks
            :when (= :backing-changed (:change b))
            :let [n       (n-rows object-type)
                  edited? (contains? (force types) object-type)]
            :when (or edited? (pos? n))]
        {:rule     :backing-replaced
         :severity (if confirmed? :warn :error)
         :entity   object-type
         :change   :backing-changed
         ::fnd/ctx {:change b :rows n :edited? edited?}})
      (for [{:keys [object-type property change] :as b} brks
            :when (and (not (#{:property-renamed? :property-renamed :backing-changed} change))
                       (if (= :primary-key-changed change)
                         (contains? (force types) object-type)
                         (contains? (force edited) [object-type property])))]
        (drop-empty
         {:rule     :migration-required
          :severity :error
          :entity   object-type
          :change   change
          :property property
          :field    (:field b)
          ::fnd/ctx {:change b :candidates (get cands [object-type property])}}))))))

(defn- say-backing-replaced
  [{{{:keys [object-type from to confirmed?]} :change n :rows edited? :edited?} ::fnd/ctx}]
  (let [t (name object-type)]
    (str "Object type " t (if confirmed? " has" " would have")
         " its storage PERMANENTLY REPLACED: its backing moves from " (pr-str from)
         " to " (pr-str to) ", and it holds " n " object(s)"
         (when edited? " and user edits")
         ". Leaving the :backing out again later unbinds the type; it does not bring"
         " the old storage back."
         (if confirmed?
           " Confirmed by :replaces in the new :backing."
           (str " To go ahead, confirm it in the new :backing with :replaces "
                (backing-token from) " — the binding it replaces, so a later change"
                " needs its own confirmation.")))))

(defn- say-migration-required
  [{{{:keys [object-type property field change from to]} :change candidates :candidates} ::fnd/ctx}]
  (let [t (name object-type)]
    (str
     (case change
       :primary-key-changed
       (str "Object type " t " changes how its ids are minted (" (pr-str from)
            " → " (pr-str to) ") and carries user edits. An edit is attached to the"
            " primary-key value it was made for, so every one of them would point"
            " at an id that no longer exists.")

       :property-type-changed
       (str "Property " (pr-str property) " on " t " changes type (" (pr-str from)
            " → " (pr-str to) ") and carries user edits. The stored edit values are"
            " of the OLD type and nothing casts them.")

       :struct-field-type-changed
       (str "Struct field " (pr-str field) " of property " (pr-str property) " on "
            t " changes type (" (pr-str from) " → " (pr-str to)
            ") and that property carries user edits.")

       :property-deleted
       (str "Property " (pr-str property) " on " t " is deleted and carries user"
            " edits — they would have nowhere to land.")

       :struct-field-deleted
       (str "Struct field " (pr-str field) " of property " (pr-str property) " on "
            t " is deleted and that property carries user edits.")

       (str "Breaking change " (pr-str change) " on " t " touches user edits."))

     (when (seq candidates)
       (str " An added property of the same type stands beside it — "
            (pr-str (vec candidates))
            " — so this MIGHT be a rename rather than a deletion; nothing in a"
            " pushed file tells the two apart."))

     (str " The only migration this platform can execute today is the 'Drop all"
          " edits': `sutra object drop-edits " t
          "` discards the edits on the WHOLE type and reconciles it to source — then"
          " push again."))))

(defn- affected-types
  [{:keys [before after] :as d}]
  (into (sorted-set-by #(compare (str %1) (str %2)))
        (filter #(and (live/holds? (:object-types before) %)
                      (live/holds? (:object-types after) %)))
        (concat (diff/changed d :object-type)
                (for [t    (diff/changed d :transform)
                      side [before after]
                      o    (tio/outputs-of (live/lookup (:transforms side) t))
                      :when (= :object-type (:kind o))]
                  (:object-type o)))))

(defn- sub-world
  [d side ts]
  (let [w  (get d side)
        tx (:transforms w)]
    {:object-types (into {} (map (juxt identity #(live/lookup (:object-types w) %))) ts)
     :transforms   (into {} (comp (mapcat #(diff/held-outputs d side :object-type %))
                                  (map first) (distinct)
                                  (map (juxt identity #(live/lookup tx %))))
                         ts)}))

(defn- changed-stranding
  [{:keys [edits] :as d}]
  (let [ts (affected-types d)]
    (sort-by (comp str :entity)
             (stranding-findings (sub-world d :after ts) (sub-world d :before ts) edits))))

(def stranded-edits
  {:find changed-stranding
   :says {:backing-replaced   say-backing-replaced
          :migration-required say-migration-required}})

(defn migration-findings
  [after live edits]
  (map #(fnd/render (:says stranded-edits) %) (stranding-findings after live edits)))
