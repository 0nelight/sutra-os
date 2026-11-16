(ns sutra.parser.lint.apps
  (:require [sutra.kit.coerce :refer [->kw]]
            [sutra.parser.lint.support :refer [top-level-page-limit ->name]]
            [clojure.string :as str]
            [sutra.kit.tree :as tree]
            [sutra.capps.events :as ce]
            [sutra.capps.variables :as vars]
            [sutra.capps.sidebar :as sb]
            [sutra.capps.view-as :as va]
            [sutra.types.versions :as vers]))

(set! *warn-on-reflection* true)

(def rules
  [{:id :var-shadows-type :severity :warn
    :doc "grammar.enta (`{:from x}`: first the variable, then the payload)"
    :scan (fn [schema]
            (for [[rid a] (sort-by (comp str key) (:applications schema))
                  :let [nm    (->name (or (:application-name a) (:name a) rid))
                        lay   (:layout a)
                        types (into #{} (map ->kw)
                                    (concat (:object-types lay) (:aggregate-types lay)
                                            (some-> (:object-type a) vector)))
                        vars  (into #{} (comp (filter (comp map? :vars))
                                              (mapcat (comp keys :vars)) (map ->kw))
                                    (tree/nodes lay))]
                  v (sort (filter types vars))]
              {:entity nm :entity-class :applications
               :message (str "App " (pr-str nm) " has a variable " v " named like the type it"
                             " binds. `{:from " v "}` reads the variable first, so every set of"
                             " " v " objects becomes whatever the variable holds. Rename the"
                             " variable (e.g. " (keyword (str (name v) "-gewaehlt")) ").")}))}

   {:id :app-outside-every-workspace :severity :error :gate? true
    :doc nil
    :scan (fn [schema]
            (let [shelves (:workspaces schema)
                  carried (into #{}
                                (mapcat (fn [[_ w]]
                                          (map ->name
                                               (keep identity
                                                     (concat (:apps w) (:anchored w)
                                                             [(:landing w)])))))
                                shelves)]
              (when (seq shelves)
                (for [[rid a] (:applications schema)
                      :let [nm (->name (or (:application-name a) (:name a)))]
                      :when (and (:promoted a)
                                 (vers/published? a)
                                 (nil? (:form-factor a))
                                 (not (carried nm))
                                 (not (carried (->name rid))))]
                  {:entity nm :entity-class :applications
                   :message (str "App " (pr-str nm) " is promoted to the portal but stands on"
                                 " no workspace. Once a platform declares its first workspace,"
                                 " the shelf is how people reach their work — a promoted app"
                                 " beside it is a second, uncurated way in, and that is the"
                                 " flat grid this split exists to end. Name it in some"
                                 " workspace's :apps (or :anchored/:landing), or drop"
                                 " :promoted so it is reached through the workspace that"
                                 " embeds it.")}))))}

   {:id :app-pages-serve-disjoint-audiences :severity :error :gate? true
    :doc nil
    :scan (fn [schema]
            (for [[_rid a] (:applications schema)
                  :let  [nm    (->name (or (:application-name a) (:name a)))
                         split (when-not (:form-factor a)
                                 (seq (va/audience-split (:layout a))))]
                  :when split
                  :let  [who (fn [g] (str/join "/" (sort (map ->name g))))]]
              {:entity nm :entity-class :applications
               :message (str "App " (pr-str nm) " splits its "
                             (name (:slot (first split))) " between audiences that do not"
                             " overlap (" (str/join " vs " (map who (:groups (first split))))
                             "). Whoever opens it sees mostly someone else's work, and the"
                             " people who share it cannot be granted it separately. That is"
                             " what a workspace is for: one shelf per group, carrying the"
                             " modules that group needs. Split the panes into apps and name"
                             " them in the workspaces that serve each audience.")}))}

   {:id :visibility-link-unknown :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[_rid a] (:applications schema)
                  :let [nm (->name (or (:application-name a) (:name a)))]
                  n (tree/nodes (:layout a))
                  :let [l (get-in n [:visibility :link])]
                  :when (and l (not (contains? (into (set (keep :object-type-name
                                                                (vals (:object-types schema))))
                                                     (keep :link-type-name)
                                                     (vals (:link-types schema)))
                                               (->kw l))))]
              {:entity nm :entity-class :applications
               :message (str "App " (pr-str nm) ": :visibility :link " (pr-str l)
                             " names no object type and no link type, so it hides nothing"
                             " from anyone. Name the type whose readers should see this.")}))}

   {:id :pane-blind-for-audience :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[_rid a] (:applications schema)
                  :let [nm (->name (or (:application-name a) (:name a)))]
                  {:keys [audience slot select]} (va/blind-audiences (:layout a))]
              {:entity nm :entity-class :applications
               :message (str "App " (pr-str nm) ": " (pr-str (->name audience))
                             " sees none of the " (name slot)
                             (when select (str " of " (pr-str select)))
                             " — every pane's :visibility leaves it out, so that"
                             " audience opens onto an empty frame. Give it at least"
                             " one pane (or drop the audience from the panes that"
                             " name it).")}))}

   {:id :pane-shows-nothing :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[_rid a] (:applications schema)
                  :let [nm (->name (or (:application-name a) (:name a)))]
                  {:keys [slot label]} (va/empty-panes (:layout a))]
              {:entity nm :entity-class :applications
               :message (str "App " (pr-str nm) ": the " (name slot) " pane "
                             (pr-str label) " has an empty :body — whoever picks it"
                             " sees a blank page. Fill it or remove the pane.")}))}

   {:id :app-page-without-padding :severity :info
    :doc "apphost/application-design-best-practices.md (padding)"
    :scan (fn [schema]
            (for [[_rid a] (:applications schema)
                  :let [l  (:layout a)
                        nm (->name (or (:application-name a) (:name a)))]
                  :when (and (map? l) (not (:form-factor a))
                             (or (:kind l) (:widget l))
                             (nil? (:padding l))
                             (not-any? :padding (:pages l)))]
              {:entity nm :entity-class :applications
               :message (str "App " (pr-str nm) " sets no :padding on its pages, so its"
                             " widgets touch the module's edges. :compact suits"
                             " most cases; set :padding on the layout or on"
                             " each page (:none says the edge-to-edge look is meant).")}))}

   {:id :view-too-many-profiles :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[_rid a] (:applications schema)
                  :when (:form-factor a)
                  :let [nm (->name (or (:application-name a) (:name a)))
                        n  (count (filter :profile? (va/audiences (:layout a))))]
                  :when (> n va/max-profiles)]
              {:entity nm :entity-class :applications
               :message (str "Object View " (pr-str nm) " names " n " profiles;"
                             " at most " va/max-profiles " are allowed per view"
                             ". Past that the"
                             " \"Viewing as\" switch stops being a choice. Merge"
                             " profiles that see the same tabs.")}))}

   {:id :open-app-misses-the-interface :severity :error :gate? true
    :doc nil
    :scan (fn [schema]
            (let [apps    (:applications schema)
                  iface   (fn [a] (vars/interface-slots (get-in a [:layout :vars])))
                  by-name (into {} (map (fn [[_ a]]
                                          [(->kw (->name (or (:application-name a) (:name a)))) a]))
                                apps)]
              (for [[_rid a] apps
                    :let  [nm (->name (or (:application-name a) (:name a)))]
                    node  (tree/nodes (:layout a))
                    e     (ce/as-vector (some #(get node %) [:on-click :on-row-click :on-change]))
                    :when (= :open-app (:do e))
                    :let  [target (get by-name (->kw (:app e)))
                           slots (when target (iface target))
                           foreign (when target (remove (set (keys slots)) (keys (:with e))))]
                    problem
                    (cond
                      (nil? target)
                      [(str "App " (pr-str nm) " öffnet " (pr-str (:app e))
                            ", und keine Anwendung heißt so. Der Sprung landet ins Leere.")]

                      (seq foreign)
                      [(str "App " (pr-str nm) " reicht "
                            (str/join ", " (map pr-str (sort foreign)))
                            " an " (pr-str (:app e)) " — das ist keine :external-id von deren"
                            " Variablen. Eine App nimmt nur an, was sie als Schnittstelle"
                            " erklärt hat"
                            (if (seq slots)
                              (str "; sie bietet " (str/join ", " (map pr-str (sort (keys slots)))))
                              ", und diese erklärt gar keine")
                            ".")]

                      :else nil)]
                {:entity nm :entity-class :applications :message problem})))}

   {:id :view-sidebar-names-nothing :severity :error :gate? true
    :doc nil
    :scan (fn [schema]
            (let [named (fn [section k]
                          (into #{} (map (fn [[r x]] (->kw (->name (or (get x k) r)))))
                                (get schema section)))
                  apps  (named :applications :application-name)
                  acts  (named :actions :action-name)]
              (for [[_rid a] (:applications schema)
                    :let  [nm (->name (or (:application-name a) (:name a)))]
                    [what x known] (concat (map (fn [x] ["App" x apps])
                                                (sb/app-names (:sidebar a)))
                                           (map (fn [x] ["Action" x acts])
                                                (sb/action-names (:sidebar a))))
                    :when (not (known x))]
                {:entity nm :entity-class :applications
                 :message (str "Object View " (pr-str nm) ": the sidebar names the " what
                               " " (pr-str (name x)) ", and none is called that — its card"
                               " would open nothing.")})))}

   {:id :view-opens-app-its-audience-cannot :severity :warn
    :doc nil
    :scan (fn [schema]
            (let [by-name (into {} (map (fn [[r x]]
                                          [(->kw (->name (or (:application-name x) r))) x]))
                                (:applications schema))
                  pub?    vers/published?
                  targets (fn [a]
                            (distinct
                             (concat
                              (sb/app-names (:sidebar a))
                              (for [n (tree/nodes (:layout a))
                                    x (concat
                                       (when (= :embed (some-> (:widget n) ->kw)) [(:app n)])
                                       (for [k [:on-click :on-row-click :on-change]
                                             e (ce/as-vector (get n k))
                                             :when (= :open-app (:do e))]
                                         (:app e)))
                                    :when x]
                                (->kw (->name x))))))]
              (for [[_rid a] (:applications schema)
                    :when (and (:form-factor a) (pub? a))
                    :let  [nm (->name (or (:application-name a) (:name a)))]
                    t     (targets a)
                    :let  [target (get by-name t)]
                    :when (and target (not (pub? target)))]
                {:entity nm :entity-class :applications
                 :message (str "Object View " (pr-str nm) " is published and opens the app "
                               (pr-str (name t)) ", which is not — whoever sees the view"
                               " sees the card, but only its editors can open it. Publish "
                               (pr-str (name t)) " or take it out of the view.")})))}

   {:id :app-page-sprawl :severity :info
    :doc nil
    :scan (fn [schema]
            (for [[_rid a] (:applications schema)
                  :let [nm    (->name (or (:application-name a) (:name a)))
                        panes (get-in a [:layout :pages])
                        n     (count panes)]
                  :when (and (nil? (:form-factor a))
                             (= :pages (some-> (get-in a [:layout :kind]) keyword))
                             (> n top-level-page-limit))]
              {:entity nm :entity-class :applications
               :message (str "App " (pr-str nm) " carries " n " top-level pages ("
                             (str/join ", " (map (comp ->name :id) panes))
                             "). The design guidance puts the overload line"
                             " at "
                             top-level-page-limit " items in a top-level navigation. Check"
                             " whether these pages are one person's workflow or several"
                             " people's — if several, they are modules on a workspace, not"
                             " pages in one app.")}))}])
