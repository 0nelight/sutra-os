(ns sutra.authoring.install.findings.actions
  (:require [sutra.types.expr :as expr]
            [clojure.string :as str]
            [sutra.authoring.install.findings :as fnd]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.live :as live]
            [sutra.kit.coerce :refer [->name-kw]]
            [sutra.parser.fn-params :as fn-params]
            [sutra.parser.spec :as spec]
            [sutra.authoring.ports :as ports]
            [sutra.catalog.functions :as functions]
            [sutra.ref.lower :as lower]
            [sutra.types.column :as column]
            [sutra.types.rid :as rid]
            [sutra.types.transform-io :as tio]
            [sutra.types.value-types :as vt]))

(set! *warn-on-reflection* true)

(defn- step-children
  [r]
  (concat (:chain r) (:steps r) (some-> (:step r) vector)
          (let [t (:then r)] (if (map? t) [t] t))
          (let [e (:else r)] (if (map? e) [e] e))))

(defn- nested-steps
  [r]
  (cons r (mapcat nested-steps (step-children r))))

(defn- expression-findings
  [schema]
  (concat
   (for [[t spec] (:objects schema)
         a     (filter sequential? (:attributes spec))
         :let [[an _ty opts] a
               ex  (get-in opts [:derived :expression])
               bad (when (vector? ex) (expr/unknown-ops ex))]
         :when (seq bad)]
     {:rule :unknown-expression-op :entity-class :object-types :entity t
      :ops (vec (sort bad))
      ::fnd/ctx {:form :derived :attribute an}})
   (for [[nm a] (:actions schema)
         [i c]  (map-indexed vector (:submission-criteria a))
         {:keys [kind binding ops]} (expr/criterion-problems c)]
     {:rule :unknown-expression-op :entity-class :actions :entity nm
      :ops ops
      ::fnd/ctx {:form kind :index i :binding binding}})))

(defn- say-unknown-op
  [{:keys [entity ops] {:keys [form attribute index binding]} ::fnd/ctx}]
  (case form
    :derived (str "Derived property " (fnd/where entity attribute) " uses "
                  (str/join ", " (map pr-str ops))
                  ", which the expression evaluator has no meaning for. It"
                  " would install, and then throw on every row — and a derived"
                  " property's throw is caught per row, so the property would"
                  " simply read NULL for ever, with no error anywhere."
                  " `:not=` is the name sutra.types.query gives `:!=`; the two"
                  " vocabularies are not the same one.")
    :pred (str "Submission criterion " (fnd/where entity :submission-criteria index :pred)
               " uses " (str/join ", " (map pr-str ops))
               ", which the expression evaluator has no meaning for. The"
               " action would install and then refuse EVERY submission with"
               " a server error instead of its own message.")
    :test (str "Submission criterion " (fnd/where entity :submission-criteria index :test binding)
               " starts with " (pr-str (first ops))
               ", and a rule argument only speaks "
               (str/join ", " (map pr-str (sort expr/binding-ref-ops)))
               ". Anything else is passed through as a LITERAL, so the"
               " predicate would compare the expression itself instead of"
               " the value it was written to read — silently, and always"
               " with the same answer.")))

(def expressions
  {:find (fn [{:keys [text]}] (expression-findings text))
   :says {:unknown-expression-op say-unknown-op}})

(defn- action-object-param-problems
  [schema]
  (let [ots (set (keys (:objects schema)))
        att (fn [ot an] (some (fn [a] (when (and (sequential? a) (= an (first a)))
                                        (second a)))
                              (:attributes (get (:objects schema) ot))))]
    (for [[nm a]      (:actions schema)
          [i rule]    (map-indexed vector (:rules a))
          [attr expr] (:values rule)
          :when       (and (vector? expr) (= :get (first expr)) (= 2 (count expr)))
          :let        [p     (second expr)
                       ptype (get-in a [:params p :type])
                       ot    (:object-type rule)]
          :when       (contains? ots ptype)]
      {:rule :object-param-into-attribute :entity-class :actions :entity nm
       :param p :param-type ptype :object-type ot :attribute attr
       ::fnd/ctx {:index i :attribute-type (att ot attr)}})))

(defn- say-object-param
  [{nm :entity :keys [param param-type object-type attribute]
    {:keys [index attribute-type]} ::fnd/ctx}]
  (str "Rule " index " of " (name nm) " writes " (pr-str [:get param])
       " into " (name object-type) "/" (name attribute)
       (when attribute-type (str ", which is declared " (pr-str attribute-type)))
       ". Parameter " (pr-str param) " is :type " (pr-str param-type)
       " — an OBJECT reference, so the rule stores the resolved"
       " object where a scalar belongs and every submission fails"
       " with a value-type error. Either read a field off it"
       " (" (pr-str [:get param :some-attribute]) ") or, if the column"
       " is a foreign key, declare the parameter :type :string and"
       " pass the key."))

(def ^:private create-ops #{:create :create-of-interface :create-or-update})

(defn- array-type? [t] (and (sequential? t) (= :array (first t))))

(defn- array-attribute?
  [schema current ot an]
  (if-let [attrs (:attributes (get (:objects schema) ot))]
    (some (fn [a] (and (sequential? a) (= an (first a)) (array-type? (second a)))) attrs)
    (some (fn [[_ spec]]
            (when (= (name ot) (some-> (:object-type-name spec) name))
              (some (fn [a] (and (= (name an) (some-> (:attribute-name a) name))
                                 (array-type? (:attribute-type a))))
                    (:attribute-list spec))))
          (:object-types current))))

(defn- create-from-list-problems
  [schema current]
  (for [[nm a]   (:actions schema)
        :let     [multi (set (keep (fn [[q spec]] (when (:multiple spec) q)) (:params a)))]
        :when    (seq multi)
        [i rule] (map-indexed vector (:rules a))
        :when    (and (create-ops (:op rule)) (not (spec/action-log-rule? rule)))
        :let     [ref (fn [v] (cond (keyword? v) v
                                    (and (vector? v) (= :get (first v))
                                         (keyword? (second v))) (second v)))
                  own (ref (:target rule))
                  arr? #(array-attribute? schema current (:object-type rule) %)
                  ps  (->> (cons (:id rule)
                                 (keep (fn [[an v]] (when-not (arr? an) v)) (:values rule)))
                           (keep ref) (filter multi) (remove #{own}) distinct)]
        :when    (seq ps)]
    {:rule :create-from-list-param :entity-class :actions :entity nm
     ::fnd/ctx {:index i :op (:op rule) :params (vec ps)}}))

(defn- say-create-from-list
  [{nm :entity {:keys [index op params]} ::fnd/ctx}]
  (let [p (first params)]
    (str "Aktion " (name nm) ", Regel " index " (" (pr-str op) ") liest "
         (str/join ", " (map pr-str params)) " — einen :multiple-Parameter, also eine LISTE."
         " Eine Regel läuft einmal je Anwendung (eine Bulk-Action wird"
         " \"einmal angewendet, mit allen Objekten\"); ein Create legt darum EIN Objekt an,"
         " mit der ganzen Liste im Wert. Je Element eines: die Regel in"
         " {:op :for-each :collection " (pr-str p) " :as :x :steps [ … [:get :x] … ]}"
         " stellen. Ändern, Löschen und Verknüpfen brauchen das nicht — sie wenden sich"
         " selbst auf jedes Objekt der Liste an.")))

(def rule-values
  {:find (fn [{schema :text :keys [current]}]
           (concat
            (for [[nm a] (:actions schema)
                  p      (expr/rule-value-problems a)]
              (assoc (select-keys p [:property :expr-op])
                     :rule :rule-value-computes :entity-class :actions :entity nm
                     ::fnd/ctx {:text (:message p)}))
            (action-object-param-problems schema)
            (create-from-list-problems schema current)))
   :says {:rule-value-computes         (fn [{nm :entity {:keys [text]} ::fnd/ctx}]
                                         (str "Action " (name nm) ": " text))
          :object-param-into-attribute say-object-param
          :create-from-list-param      say-create-from-list}})

(def ^:private targeted-rule-ops
  #{:update :delete-object :update-of-interface :delete-of-interface :create-or-update})

(def ^:private scalar-param-types
  (into #{:time :time-series :struct :list :map :object-set} (keys vt/value-types)))

(defn- rule-ref-param
  [v]
  (cond
    (keyword? v) v
    (and (vector? v) (= :get (first v)) (keyword? (second v))
         (or (= 2 (count v)) (and (= 3 (count v)) (= :id (nth v 2)))))
    (second v)))

(defn- target-slots
  [{:keys [op] :as rule}]
  (cond
    (contains? targeted-rule-ops op)
    (when-not (and (= :create-or-update op) (nil? (:target rule)))
      [[:target (:target rule)]])
    (#{:create-link :create-link-of-interface} op)
    [[:from-id (:from-id rule)] [:to-id (:to-id rule)]]))

(defn- target-findings
  [{schema :text :keys [unchanged] :as d}]
  (let [kw        ->name-kw
        either    (fn [text-keys kk]
                    (let [authored (set (map kw text-keys))
                          live     (diff/known d kk)]
                      (fn [t] (or (contains? authored t) (boolean (live t))))))
        obj-types (either (keys (:objects schema)) :object-types)
        ifaces    (either (keys (:interfaces schema)) :interfaces)
        unchanged (into #{} (keep (fn [[k e]] (when (= :action k) (kw e)))) unchanged)
        vts       (either (keys (:value-types schema)) :value-types)
        scalars   (fn [t] (or (contains? scalar-param-types t) (vts t)))
        ptype     (fn [spec] (kw (or (:object-type spec) (:type spec))))
        object-param? (fn [spec] (let [t (ptype spec)]
                                   (and t (or (obj-types t) (ifaces t) (not (scalars t))))))]
    (for [[nm a]   (:actions schema)
          :let     [params   (:params a)
                    rules    (vec (:rules a))
                    settled? (contains? unchanged (kw nm))]
          [i rule] (map-indexed vector rules)
          :let     [as-bound (set (keep :as (subvec rules 0 i)))]
          [slot v] (target-slots rule)
          :let     [p      (rule-ref-param v)
                    spec   (get params p)
                    ot     (kw (:object-type rule))
                    pt     (when spec (ptype spec))
                    reason (cond
                             (and p (contains? as-bound p)) nil
                             (nil? p)
                             (if (and (vector? v) (= :get (first v)) (keyword? (second v)))
                               :reads-a-value
                               :not-a-parameter)
                             (nil? spec) :unknown-parameter
                             (not (object-param? spec)) :scalar-parameter
                             (and (= :target slot) ot (obj-types pt)
                                  (not (ifaces pt)) (not= ot pt))
                             :other-type)]
          :when    reason]
      {:rule :rule-target-not-object-param :entity-class :actions :entity nm
       :severity (if settled? :warn :error)
       :rule-index i :op (:op rule) :slot slot :target v
       ::fnd/ctx {:reason reason :param p :param-type pt :object-type ot :settled? settled?}})))

(defn- say-target
  [{nm :entity :keys [rule-index op slot target]
    {:keys [reason param param-type object-type settled?]} ::fnd/ctx}]
  (let [why (case reason
              :reads-a-value     (str "liest " (pr-str target) " — einen WERT des Parameters "
                                      (pr-str (second target)) ", kein Objekt")
              :not-a-parameter   (str "ist " (pr-str target) " — kein Objektparameter")
              :unknown-parameter (str "nennt " (pr-str param)
                                      ", und das ist kein Parameter der Aktion")
              :scalar-parameter  (str "nennt Parameter " (pr-str param) " vom Typ "
                                      (pr-str param-type)
                                      " — ein Wert, keine Objektreferenz")
              :other-type        (str "nennt Parameter " (pr-str param) " vom Typ "
                                      (pr-str param-type)
                                      ", die Regel ändert aber " (pr-str object-type)))]
    (str "Aktion " (name nm) ", Regel " rule-index " (" (pr-str op)
         (when object-type (str " " (name object-type))) "): " (pr-str slot) " " why ". "
         "Eine Regel ändert, löscht oder verknüpft nur Objekte, die als"
         " Objektparameter übergeben werden ("
         "\"derived from object reference parameters\") — ein Ziel wie "
         (pr-str target) " ist ein Wert: welches Objekt er meint, zeigt sich erst"
         " beim Absenden, und bisher schrieb die Regel dann stillschweigend"
         " nichts. Stattdessen: das Zielobjekt als"
         " eigenen Objektparameter deklarieren (`:params {:x {:type "
         (if object-type (str object-type) "<typ>") "}}`, Ziel `[:get :x]`) und dort"
         " binden, wo die Aktion platziert ist (`:prefill`/`:defaults` in"
         " der .enta/.entv)."
         (when settled?
           (str " Schon installiert und unverändert — deshalb nur eine"
                " Warnung; die nächste Änderung an dieser Aktion wird"
                " abgelehnt.")))))

(def rule-targets
  {:find target-findings
   :says {:rule-target-not-object-param say-target}})

(def ^:private removed-writeback-ops #{:writeback-create :writeback-update :writeback-delete})

(defn- webhook-findings
  [schema]
  (for [[nm a] (:actions schema)
        :let [rules (vec (:rules a))
              wb    (filterv #(= :writeback (:phase %)) rules)
              top   (set rules)
              steps (mapcat nested-steps rules)]
        p (concat
           (for [s steps :when (removed-writeback-ops (:op s))]
             {:rule :writeback-verb-removed ::fnd/ctx {:op (:op s)}})
           (for [s steps
                 :when (and (= :invoke-webhook (:op s))
                            (not (and (contains? top s) (#{:writeback :side-effect} (:phase s)))))]
             {:rule :webhook-phase-required})
           (when (< 1 (count wb))
             [{:rule :multiple-writeback-rules ::fnd/ctx {:count (count wb)}}])
           (for [r wb :when (not= :invoke-webhook (:op r))]
             {:rule :writeback-phase-invalid ::fnd/ctx {:op (:op r)}})
           (for [s steps :when (lower/entity-ops (:op s))]
             {:rule :schema-edit-in-action ::fnd/ctx {:op (:op s)}}))]
    (assoc p :entity-class :actions :entity nm)))

(def webhooks
  {:find (fn [{:keys [text]}] (webhook-findings text))
   :self #{:writeback-verb-removed :webhook-phase-required :multiple-writeback-rules
           :writeback-phase-invalid :schema-edit-in-action}
   :says
   {:writeback-verb-removed
    (fn [{nm :entity {:keys [op]} ::fnd/ctx}]
      (str (name nm) ": " op " is gone. Write through a webhook of the"
           " source in the writeback position — {:op :invoke-webhook :phase :writeback"
           " :source … :webhook … :params {…} :as :wb} — and edit the object with an"
           " ordinary rule that reads the answer, e.g. [:get :wb :identity]. A push after"
           " the action's edits is {:op :invoke-webhook :phase :side-effect …}."))

    :webhook-phase-required
    (fn [{nm :entity}]
      (str (name nm) ": a webhook is a rule of its own with :phase :writeback (before"
           " every other rule, its answer usable after it, a failure aborts) or"
           " :phase :side-effect (after the commit, best effort) — the two"
           " positions."))

    :multiple-writeback-rules
    (fn [{nm :entity {n :count} ::fnd/ctx}]
      (str (name nm) " has " n " rules in the writeback position; an action"
           " has at most one writeback webhook. Several rows of one"
           " table go through one call with a list input; anything else is a"
           " :phase :side-effect webhook."))

    :writeback-phase-invalid
    (fn [{nm :entity {:keys [op]} ::fnd/ctx}]
      (str (name nm) ": only an :invoke-webhook can stand in the writeback position,"
           " not " op "."))

    :schema-edit-in-action
    (fn [{nm :entity {:keys [op]} ::fnd/ctx}]
      (str (name nm) ": " op " changes the ontology; an action edits objects and"
           " links only. Change the schema with an install."))}})

(defn- source-webhook-findings
  [schema]
  (concat
   (for [[src-nm src] (:sources schema)
         [w d] (:webhooks src)
         :when (and (map? d) (= :table (->name-kw (:kind d))))
         r     (or (seq (:requests d)) [d])
         :let [table (:table r)
               op    (->name-kw (:op r))]
         :when (or (not (contains? (:writeback src) table))
                   (not (#{:insert :update :delete} op)))]
     {:rule :webhook-table-invalid :entity-class :source :entity src-nm
      ::fnd/ctx {:webhook w :allowlist (vec (keys (:writeback src))) :table table :op op}})
   (for [[src-nm src] (:sources schema)
         [w d] (:webhooks src)
         :when (and (map? d) (= :table (->name-kw (:kind d))) (seq (:requests d)))
         :let [ases (mapv (comp ->name-kw :as) (:requests d))]
         [i r] (map-indexed vector (:requests d))
         :let [before (set (take i ases))
               bad    (cond
                        (nil? (nth ases i))                 "names no :as"
                        (some #{(nth ases i)} (take i ases)) "repeats an earlier :as"
                        :else (some (fn [[_ [src]]]
                                      (when-not (before (->name-kw src))
                                        (str "reads :from " (pr-str src)
                                             ", which is no earlier request")))
                                    (:from r)))]
         :when bad]
     {:rule :webhook-request-invalid :entity-class :source :entity src-nm
      ::fnd/ctx {:webhook w :request i :problem bad}})))

(def source-webhooks
  {:find (fn [{:keys [text]}] (source-webhook-findings text))
   :says {:webhook-table-invalid
          (fn [{src-nm :entity {:keys [webhook allowlist table op]} ::fnd/ctx}]
            (str "Source " (name src-nm) " webhook " (name webhook) ": a :table webhook"
                 " names a table of the source's :writeback allowlist (" (pr-str allowlist)
                 ") and :op :insert, :update or :delete — got :table " (pr-str table)
                 " :op " (pr-str op) "."))
          :webhook-request-invalid
          (fn [{src-nm :entity {:keys [webhook request problem]} ::fnd/ctx}]
            (str "Source " (name src-nm) " webhook " (name webhook) ": request " request
                 " " problem " — each request of a :requests chain names its :as, and"
                 " :from {column [as key]} reads only a request before it."))}})

(defn- writeback-foreign-col
  [cfg attr]
  (let [local (name (column/attr->col attr))
        m     (:column-map cfg)]
    (name (or (get m local) (get m (keyword local)) local))))

(defn- writeback-pk-findings
  [{schema :text :keys [before after] :as d}]
  (let [staged-ot  (diff/staged d :object-type)
        {:keys [writeback-of]} (diff/index d)
        nk         (rid/name-field-of :sources)
        held       (for [ot (keys staged-ot)
                         r  (distinct (mapcat writeback-of (diff/live-keys d ot)))]
                     (or (get-in d [:current :sources r nk]) r))
        after-objs (:object-types after)]
    (for [src-nm (sort-by str (distinct (concat (keys (:sources schema)) held)))
          :let   [src (if (contains? (:sources schema) src-nm)
                        (get (:sources schema) src-nm)
                        (live/lookup (:sources before) src-nm))]
          [table cfg]  (:writeback src)
          :let  [ot  (->name-kw (:object-type cfg))
                 obj (live/lookup after-objs ot)
                 pk  (some-> (:pk cfg) str)]
          :when (and obj pk
                     (or (contains? (:sources schema) src-nm) (contains? staged-ot ot)))
          :let  [cols (into {} (map (fn [a] [(str/lower-case (writeback-foreign-col cfg a)) a]))
                            (diff/attr-names-of obj))]
          :when (not (contains? cols (str/lower-case pk)))]
      {:rule :writeback-pk-unreachable :entity-class :source :entity src-nm
       :table table :object-type ot
       ::fnd/ctx {:pk pk}})))

(defn- say-writeback-pk
  [{src-nm :entity ot :object-type :keys [table] {:keys [pk]} ::fnd/ctx}]
  (str "Source " (name src-nm) " allowlists " (pr-str table) " for " (name ot)
       " with :pk " (pr-str pk) ", but no attribute of " (name ot)
       " reaches that column: each attribute folds to its column name, then"
       " the allowlist's :column-map renames it. Every action that writes"
       " back to " (name ot) " would fail before its first rule. Map the key"
       " attribute onto it — :column-map {\"<folded attribute>\" " (pr-str pk)
       "} (a :column-map target may be the :pk; it is resolved, never written)."))

(def writeback-pk
  {:find writeback-pk-findings
   :says {:writeback-pk-unreachable say-writeback-pk}})

(def ^:private edit-ops
  (into (conj lower/write-ops :create-or-update) lower/expand-ops))

(defn- edits-in
  [actions seen r]
  (let [callee (when (#{:call :effect} (:op r)) (:fn r))]
    (concat (when (edit-ops (:op r)) [(:op r)])
            (mapcat #(edits-in actions seen %) (step-children r))
            (when (and callee (not (seen callee)))
              (mapcat #(edits-in actions (conj seen callee) %) (:rules (get actions callee)))))))

(def side-effect-edits
  {:find (fn [{schema :text}]
           (let [actions (:actions schema)]
             (for [[nm a] actions
                   [i r]  (map-indexed vector (:rules a))
                   :when  (= :side-effect (:phase r))
                   :let   [ops (distinct (edits-in actions #{nm} r))]
                   :when  (seq ops)]
               {:rule :side-effect-edit :entity-class :actions :entity nm :rule-index i
                ::fnd/ctx {:ops ops}})))
   :says {:side-effect-edit
          (fn [{nm :entity i :rule-index {:keys [ops]} ::fnd/ctx}]
            (str (name nm) " rule " i " runs in the side-effect phase and edits the ontology"
                 " (" (str/join ", " (map name ops)) "). A side effect only sends — a"
                 " notification, a webhook, a writeback push — after the action's edits;"
                 " move the edit into the action's rules."))}})

(def edit-functions
  {:find (fn [{schema :text :keys [current]}]
           (let [fns     (:functions schema)
                 fn-of   (functions/library-lookup fns current)
                 compile (ports/code-compile-clojure-edit)]
             (concat
              (for [[nm f] fns
                    :let [why (functions/uses-problems fn-of nm f)]
                    :when (seq why)]
                {:rule :function-uses-unbound :entity-class :functions :entity nm
                 ::fnd/ctx {:why why}})
              (for [[nm f] fns
                    :when (and (= :clojure (->name-kw (:language f))) (string? (:source f)))
                    over (fn-params/over-limit (:source f))]
                {:rule :function-too-many-params :entity-class :functions :entity nm
                 ::fnd/ctx over})
              (when compile
                (for [[nm f] fns
                      :let [src (:source f)]
                      :when (and (= :clojure (->name-kw (:language f))) (string? src)
                                 (or (:library f) (str/includes? src ":sutra/function"))
                                 (empty? (functions/uses-problems fn-of nm f))
                                 (empty? (fn-params/over-limit src)))
                      :let [why (compile (functions/function-code fn-of nm f))]
                      :when why]
                  {:rule :function-does-not-compile :entity-class :functions :entity nm
                   ::fnd/ctx {:why why}})))))
   :says {:function-too-many-params
          (fn [{nm :entity {:keys [params] fname :name} ::fnd/ctx}]
            (str "edit function " (name nm) ": " (if fname (str "`" fname "`") "a fn")
                 " declares " params " parameters; Clojure takes at most " fn-params/limit
                 " — drop the ones it can derive, or move the logic into a :library function"
                 " that takes a map, and call that from the body"))
          :function-does-not-compile
          (fn [{nm :entity {:keys [why]} ::fnd/ctx}]
            (str "edit function " (name nm) " does not compile in the sandbox: " why))
          :function-uses-unbound
          (fn [{{:keys [why]} ::fnd/ctx}]
            (str (str/join "; " why) ". A function binds with :uses the libraries"
                 " (:library true, Clojure) of its own repository."))}})

(defn- fk-link-outputs
  [transforms link-name]
  (for [[_ t] transforms
        o (tio/outputs-of t)
        :when (and (= :link-type (->name-kw (:kind o))) (:fk-attribute o))]
    (keyword (link-name (:link-type o)))))

(defn- constraint-realisations
  [schema i c]
  (into (if (some #(= c (->name-kw (first %))) (:links schema)) #{c} #{})
        (for [l (:links schema)
              s (let [x (:satisfies (get l 4))] (cond (sequential? x) x x [x]))
              :when (and (= i (->name-kw (:interface s))) (= c (->name-kw (:constraint s))))]
          (->name-kw (first l)))))

(defn- written-links
  [schema r]
  (case (->name-kw (:op r))
    (:create-link :delete-link) [(->name-kw (:link-type r))]
    (:create-link-of-interface :delete-link-of-interface)
    (constraint-realisations schema (->name-kw (:interface r)) (->name-kw (:constraint r)))
    nil))

(defn- derived-links
  [{schema :text :keys [current] :as d}]
  (let [authored  (set (fk-link-outputs (:transforms schema) identity))
        link-name #(or (get-in current [:link-types % :link-type-name]) %)]
    (fn [lt]
      (or (contains? authored lt)
          (boolean
           (some #(= lt %)
                 (fk-link-outputs (into {} (for [r (diff/live-keys d lt)
                                                 t ((:fk-link (diff/index d)) r)]
                                             [t (get-in current [:transforms t])]))
                                  link-name)))))))

(defn- derived-link-write-findings
  [{schema :text :as d}]
  (let [derived (derived-links d)]
    (for [[nm a] (:actions schema)
          r (mapcat nested-steps (:rules a))
          :when (map? r)
          lt (written-links schema r)
          :when (derived lt)]
      {:rule :derived-link-written :entity-class :actions :entity nm :severity :error
       ::fnd/ctx {:op (:op r) :link-type lt}})))

(def derived-link-writes
  {:find derived-link-write-findings
   :says {:derived-link-written
          (fn [{nm :entity {:keys [op link-type]} ::fnd/ctx}]
            (str "Aktion " (name nm) ": " (name op) " auf " (name link-type)
                 " — dieser Link folgt einem FK-Attribut und wird nicht geschrieben;"
                 " ändere das Attribut"))}})

(defn- live-types
  [{:keys [current] :as d}]
  (live/by-field current (:names (diff/index d)) :object-types :object-type-name (fn [_ o] o)
                 (delay (live/by-name (for [[r o] (:object-types current)]
                                        [(:object-type-name o) r o])))))

(defn- action-log-findings
  [{schema :text :as d}]
  (let [live      (live-types d)
        type-of   (fn [t] (or (get-in schema [:objects t]) (live/lookup live t)))
        required  (fn [t] (boolean (:requires-action-log (if (contains? (:objects schema) t)
                                                            (get-in schema [:objects t])
                                                            (live/lookup live t)))))
        log-type? (fn [t] (let [o (type-of t)]
                            (boolean (some #(some-> % (str/starts-with? "[LOG] "))
                                           [(:name o) (:plural o)]))))
        logged?   (fn [a] (or (:action-log a)
                              (some #(and (= :create (:op %)) (log-type? (:object-type %)))
                                    (:rules a))))]
    (for [[nm a] (:actions schema)
          :let [touched (distinct (for [r (:rules a)
                                        :let [t (:object-type r)]
                                        :when (required t)]
                                    t))]
          :when (and (seq touched) (not (logged? a)))]
      {:rule :action-log-required :entity-class :actions :entity nm
       :object-types (vec touched)})))

(defn- say-action-log
  [{nm :entity :keys [object-types]}]
  (str (name nm) " bearbeitet "
       (str/join ", " (map name object-types))
       " — und dort ist das Action-Log Pflicht (:requires-action-log)."
       " Ohne Log bleibt eine Änderung an diesem Typ ohne Urheber und"
       " ohne Zeitpunkt, und genau das soll der Schalter verhindern."
       " Setze :action-log true an der Action; der Rest entsteht von"
       " selbst (Objekttyp, Parameter, Regel und die Kante auf das"
       " bearbeitete Objekt)."))

(defn- function-log-findings
  [{schema :text}]
  (for [[nm a] (:actions schema)
        :when (and (:action-log a) (spec/function-rule a) (empty? (:edits a)))]
    {:rule :action-log-needs-edits :entity-class :actions :entity nm}))

(defn- say-function-log
  [{nm :entity}]
  (str (name nm) " loggt eine Funktion, nennt aber keine :edits. Der Log verknuepft die "
       "bearbeiteten Objekte ueber ihre Typen; ohne :edits weiss er nicht, welche das sind "
       "."))

(def action-log
  {:find (fn [ctx] (concat (action-log-findings ctx) (function-log-findings ctx)))
   :says {:action-log-required say-action-log
          :action-log-needs-edits say-function-log}})
