(ns sutra.parser.lint.references
  (:require [sutra.parser.lint.support :refer [app-description-limit ->name section-entries
                                               ot-name]]
            [clojure.string :as str]
            [sutra.kit.tree :as tree]
            [sutra.capps.writeback :as wb]
            [sutra.types.authority :as authority]
            [sutra.types.rid :as rid]
            [sutra.types.submission :as csub]))

(set! *warn-on-reflection* true)

(def rules
  [{:id :input-unreferenced :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [{:keys [transform project input from]} (authority/unreferenced-inputs schema)]
              {:entity (->name transform) :entity-class :transforms
               :message (str "Transform " (pr-str (->name transform)) " reads " (pr-str input)
                             " from project " (pr-str from) ", which its project "
                             (pr-str project) " neither references (:references) nor imports"
                             " (:imports). A read across projects is a decision, as a"
                             " project reference is: reference the resource, or import the"
                             " project.")}))}

   {:id :app-description-length :severity :error
    :doc nil
    :scan (fn [schema]
            (for [[_rid a] (:applications schema)
                  :let [nm (->name (or (:application-name a) (:name a)))
                        d  (:description a)]
                  :when (and (string? d) (> (count d) app-description-limit))]
              {:entity nm :entity-class :applications
               :message (str "App " (pr-str nm) " has a " (count d) "-character description"
                             " — the launcher card serves at most " app-description-limit
                             " (`my-applications` truncates the rest away, so everything past"
                             " character " app-description-limit
                             " reaches no reader). Shorten it to the"
                             " blurb a card can show; long-form documentation belongs in the"
                             " app's own layout (a `:markdown` widget) where it is actually read.")}))}

   {:id :action-button-cannot-submit :severity :error
    :doc nil
    :scan (fn [schema]
            (let [white  (fn [a] (or (get-in a [:layout :actions])
                                     (some-> (get-in a [:layout :action]) vector)))
                  needed (fn [act] (set (keys (filter (comp csub/client-gated? val)
                                                      (get-in schema [:actions act :params])))))
                  starved
                  (for [[_rid a] (:applications schema)
                        :let [nm (->name (or (:application-name a) (:name a)))]
                        entry (white a)
                        :let [act  (some-> (:action entry) keyword)
                              allow (some->> (:parameters entry) (map keyword) set)
                              miss  (when (and act allow)
                                      (sort (map ->name (remove allow (needed act)))))]
                        :when (seq miss)]
                    {:entity (str nm "." (->name act)) :entity-class :applications
                     :message (str "App " (pr-str nm) " whitelists " (pr-str (->name act))
                                   " but its `:parameters` omit the required param(s) "
                                   (pr-str (vec miss)) ". The server takes an Action's"
                                   " arguments ONLY from this whitelist, so those params can"
                                   " never arrive and every submit is refused before any edit."
                                   " Add them to `:parameters`.")})]
              (concat
               starved
               (for [[_rid a] (:applications schema)
                     :let [nm    (->name (or (:application-name a) (:name a)))
                           wl    (white a)
                           names (set (keep #(some-> (:action %) keyword) wl))
                           only  (when (= 1 (count names)) (first names))]
                     node (tree/nodes (:layout a))
                     :when (= :action-button (some-> (:widget node) keyword))
                     :let [act  (or (some-> (:action node) keyword) only)
                           decl (get-in schema [:actions act :params])
                           need (set (keys (filter (comp csub/client-gated? val) decl)))
                           miss (sort (map ->name (remove (set (keys (:params node))) need)))]
                     :when (or (nil? act) (not (contains? names act)) (seq miss))]
                 {:entity (str nm "." (or (->name act) "?")) :entity-class :applications
                  :message
                  (cond
                    (nil? act)
                    (str "App " (pr-str nm) " has an :action-button that names no action and the"
                         " app whitelists " (count names) " — the button cannot know which Action"
                         " to submit and the server picks one. Name it (`:action`).")
                    (not (contains? names act))
                    (str "App " (pr-str nm) " has an :action-button for " (pr-str (->name act))
                         ", which the app does not whitelist (`:layout :action`/`:actions`)."
                         " The server refuses every submit from this button — it is dead on the"
                         " page and says nothing when pressed.")
                    :else
                    (str "App " (pr-str nm) "'s :action-button for " (pr-str (->name act))
                         " binds no value for its required param(s) " (pr-str (vec miss))
                         ". The Action declares them `:required`, so every submit from this"
                         " button is refused before any edit. Bind them in `:params`, or drop"
                         " the requirement at the Action."))}))))}

   {:id :deprecated-without-metadata :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[cls name-field] rid/name-field-of
                  [nm spec] (section-entries (get schema cls) name-field)
                  :when (and (= :deprecated (some-> (:status spec) ->name keyword))
                             (empty? (:deprecation spec)))]
              {:entity nm :entity-class cls
               :message (str (pr-str nm) " is deprecated and says nothing else. A"
                             " deprecation carries three things — WHY it is going,"
                             " WHEN it is expected to be deleted, and WHAT replaces"
                             " it — and without them the status is a dead end: the"
                             " next author sees a warning and no way forward, so they"
                             " use it anyway. Declare `:deprecation {:reason \"…\""
                             " :deadline \"YYYY-MM-DD\" :replacement :other-name}`.")}))}

   {:id :reference-to-deprecated :severity :warn
    :doc nil
    :scan (fn [schema]
            (let [dep  (into {}
                             (for [[cls name-field] rid/name-field-of
                                   [nm spec] (section-entries (get schema cls) name-field)
                                   :when (= :deprecated (some-> (:status spec) ->name keyword))]
                               [nm (some-> (get-in spec [:deprecation :replacement]) ->name)]))
                  say  (fn [target]
                         (if-let [r (get dep target)]
                           (str " Use " (pr-str r) " instead.")
                           " It declares no replacement, so ask before building on it."))]
              (concat
               (for [[nm spec] (section-entries (:actions schema) :action-name)
                     [pn pspec] (:params spec)
                     :let [t (some->> (:type pspec) (ot-name schema))]
                     :when (contains? dep t)]
                 {:entity (str nm "." (->name pn)) :entity-class :actions
                  :message (str "Action " (pr-str nm) " takes " (pr-str (->name pn))
                                " as " (pr-str t) ", which is deprecated." (say t))})
               (for [[_rid a] (:custom-applications schema)
                     :let [nm (->name (:custom-application-name a))]
                     t     (get-in a [:scope :object-types])
                     :let  [tn (ot-name schema t)]
                     :when (contains? dep tn)]
                 {:entity (str nm "." tn) :entity-class :custom-applications
                  :message (str "Custom application " (pr-str nm) " scopes "
                                (pr-str tn) ", which is deprecated — its SDK is cut"
                                " from this list, so the stanza ships to a client"
                                " outside the platform." (say tn))})
               (for [[_rid a] (:applications schema)
                     :let [nm (->name (or (:application-name a) (:name a)))
                           tn (some-> (:object-type a) (->> (ot-name schema)))]
                     :when (contains? dep tn)]
                 {:entity (str nm "." tn) :entity-class :applications
                  :message (str "App " (pr-str nm) " binds " (pr-str tn)
                                ", which is deprecated." (say tn))}))))}

   {:id :app-binds-unknown-type :severity :error
    :doc nil
    :scan (fn [schema]
            (let [known (into #{}
                              (mapcat (fn [[section name-field]]
                                        (mapcat (fn [[rid spec]]
                                                  [(->name rid) (->name (get spec name-field))])
                                                (get schema section))))
                              {:object-types :object-type-name
                               :interfaces   :interface-name})]
              (for [[_rid a] (:applications schema)
                    :let [nm (->name (or (:application-name a) (:name a)))]
                    slot  [:object-types :aggregate-types]
                    t     (get-in a [:layout slot])
                    :let  [tn (->name t)]
                    :when (and tn (not (contains? known tn)))]
                {:entity (str nm "." tn) :entity-class :applications
                 :message (str "App " (pr-str nm) " binds " (pr-str tn) " in `:layout "
                               slot "`, which is neither an object type nor an interface."
                               " The payload resolves each entry by name, so every widget"
                               " reading it renders EMPTY — the app looks like it has no"
                               " data instead of saying it cannot find its type. Name a"
                               " declared type, or drop the entry.")})))}

   {:id :custom-app-unknown-reference :severity :error
    :doc nil
    :scan (fn [schema]
            (let [known (fn [section name-field]
                          (into #{} (comp (mapcat (fn [[k spec]]
                                                    [k (:id spec) (get spec name-field)]))
                                          (keep #(some-> % ->name)))
                                (get schema section)))
                  sets  {:object-types (known :object-types :object-type-name)
                         :actions      (known :actions :action-name)
                         :projects     (known :projects :project-name)
                         :repositories (known :repositories :repository-name)}]
              (for [[_rid a] (:custom-applications schema)
                    :let [nm (->name (:custom-application-name a))]
                    [field section refs] [[":scope :object-types" :object-types
                                           (get-in a [:scope :object-types])]
                                          [":scope :actions" :actions (get-in a [:scope :actions])]
                                          [":project" :projects (some-> (:project a) vector)]
                                          [":repository" :repositories
                                           (some-> (:repository a) vector)]]
                    r     refs
                    :let  [rn (->name r)]
                    :when (and rn (not (contains? (sets section) rn)))]
                {:entity (str nm "." rn) :entity-class :custom-applications
                 :message (str "Custom application " (pr-str nm) " names " (pr-str rn)
                               " in `" field "`, but no " (name section) " entry has that"
                               " name. Its SDK and its token scope are cut from what it"
                               " names, so the client outside the platform finds the"
                               " entry missing at its first call. Name a declared one,"
                               " or drop it.")})))}

   {:id :app-client-join :severity :error
    :doc nil
    :scan (fn [schema]
            (for [[_rid a] (:applications schema)
                  :let [nm (->name (or (:application-name a) (:name a)))]
                  node (tree/nodes (:layout a))
                  [v-name v-spec] (:vars node)
                  :let [j (:join v-spec)]
                  :when (map? j)]
              {:entity (str nm "." (->name v-name)) :entity-class :applications
               :message (str "App variable " (pr-str (->name v-name)) " in " (pr-str nm)
                             " joins " (pr-str (->name (or (:source v-spec) (:from v-spec))))
                             " to " (pr-str (->name (:index j)))
                             " on " (pr-str (->name (:on j))) "="
                             (pr-str (->name (:key j)))
                             " in the CLIENT. nothing can group by it, select across it, or"
                             " check it. Declare the link-type between the two object types and"
                             " read across it (`:group-by [[:link <link-type>]]`, or a :derived"
                             " property).")}))}

   {:id :writeback-form-hand-built :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[_rid a] (:applications schema)
                  :let [nm (->name (or (:application-name a) (:name a)))]
                  {:keys [action params fields widgets]} (wb/hand-built-forms (:layout a))]
              {:entity (str nm "." (->name (or action "?"))) :entity-class :applications
               :message (str "App " (pr-str nm) " restates the form for Action "
                             (pr-str (->name (or action "?"))) " — " (count fields)
                             " input widget(s) feed nothing but its params "
                             (pr-str (mapv #(keyword (->name %)) params))
                             ". Use `{:widget :action-form :action " (pr-str action)
                             "}`, which generates that form from the Action definition"
                             ". The Action already declares every one"
                             " of those params — type, label, requiredness, constraints — so the"
                             " widgets (" (str/join ", " (map #(str (->name %)) widgets))
                             ") are the writeback interface written a second time, in a second"
                             " place, and it drifts from the Action the moment one of them"
                             " changes. `:action-button` stays right for a submit whose values"
                             " the app already holds.")}))}])
