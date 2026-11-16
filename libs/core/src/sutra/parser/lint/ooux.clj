(ns sutra.parser.lint.ooux
  (:require [sutra.kit.coerce :refer [->kw]]
            [sutra.parser.lint.support :refer [->name section-entries title-attr-re type-ref
                                               user-types attr-hidden? link-ends implementers
                                               action-managed-types identifier-re title-key-of
                                               key-title? readable-attr? per-period-re
                                               status-name-re attribute-priority-limit type-labels
                                               override-findings]]
            [clojure.string :as str]
            [sutra.parser.spec :as spec]
            [sutra.types.lifecycle :as lifecycle]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(def rules
  [{:id :title-is-identifier :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[nm spec] (user-types schema)
                  :let [tk (title-key-of spec)]
                  :when tk
                  :let [pick (first (for [a (:attribute-list spec)
                                          :let [an (->name (:attribute-name a))]
                                          :when (and (not= tk an)
                                                     (re-find title-attr-re an)
                                                     (readable-attr? spec a))]
                                      an))]
                  :when (or (re-find identifier-re tk)
                            (and pick (key-title? spec tk)))]
              {:entity nm :entity-class :object-types
               :message (str "Object type " (pr-str nm) " is titled by " (pr-str tk)
                             ", an identifier — lists and pickers then show numbers. Title it"
                             " with " (if pick
                                        (str "`:title :" pick "`")
                                        "a readable property (or a :derived one composed of it)")
                             " and move the number to `:subtitle :" tk "`.")}))}

   {:id :object-type-unnamed :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[nm spec] (user-types schema)
                  :when (str/blank? (:display-name spec))]
              {:entity nm :entity-class :object-types
               :message (str "Object type " (pr-str nm) " has no :name. Name it in their"
                             " words.")}))}

   {:id :app-function-unknown :severity :warn
    :doc "an app that names a function no one declares fails when it calls it"
    :scan (fn [schema]
            (let [known (into #{}
                              (mapcat (fn [[k f]] [(->name k) (->name (:function-name f))]))
                              (:functions schema))]
              (for [[_rid a] (:custom-applications schema)
                    :let [nm (->name (:custom-application-name a))]
                    f (concat (get-in a [:layout :functions]) (get-in a [:scope :functions]))
                    :when (not (known (->name f)))]
                {:entity (str nm "." (->name f)) :entity-class :custom-applications
                 :message (str "App " (pr-str nm) " names function " (pr-str (->name f))
                               ", which is declared nowhere. Calling it will fail.")})))}

   {:id :object-type-plural-missing :severity :error :gate? true
    :doc nil
    :scan (fn [schema]
            (for [[nm spec] (user-types schema)
                  :when (str/blank? (:plural spec))]
              {:entity nm :entity-class :object-types
               :message (str "Object type " (pr-str nm) " has no :plural. Every list, link"
                             " section and picker that shows many of them names them by it —"
                             " without one a list of many reads as one (\"Rechnungsposition\""
                             " over ten rows). Set `:plural \"…\"` in their words.")}))}

   {:id :object-type-undescribed :severity :info
    :doc nil
    :scan (fn [schema]
            (for [[nm spec] (user-types schema)
                  :when (str/blank? (:description spec))]
              {:entity nm :entity-class :object-types
               :message (str "Object type " (pr-str nm) " has no :description — a one-sentence"
                             " answer to \"what is one of these?\", shown where people search.")}))}

   {:id :object-type-isolated :severity :warn
    :doc nil
    :scan (fn [schema]
            (let [impl    (implementers schema)
                  linked  (into #{} (for [{:keys [from to]} (link-ends schema)
                                          end [from to]
                                          n (cons end (get impl end))]
                                      n))
                  bound   (into #{} (for [[k i] (:interfaces schema)
                                          :when (seq (:link-constraints i))
                                          n (get impl (type-ref schema k))]
                                      n))]
              (for [users [(user-types schema)]
                    :when (< 1 (count users))
                    [nm _] users
                    :when (not (or (linked nm) (bound nm)))]
                {:entity nm :entity-class :object-types
                 :message (str "Object type " (pr-str nm) " is linked to nothing — no"
                               " link type touches it and no interface it implements"
                               " constrains a link. An object a person cannot navigate to or"
                               " from is a table, not an object; link it to what it belongs to.")})))}

   {:id :attributes-unprioritized :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[nm spec] (user-types schema)
                  :let [vis (remove attr-hidden? (:attribute-list spec))]
                  :when (and (> (count vis) attribute-priority-limit)
                             (empty? (:row spec))
                             (not-any? #(= :prominent (->kw (get-in % [:opts :visibility]))) vis))]
              {:entity nm :entity-class :object-types
               :message (str "Object type " (pr-str nm) " shows " (count vis)
                             " properties, all equal — nothing says which few matter. Mark"
                             " them `:visibility :prominent` and give the type a `:row [...]`;"
                             " hide the technical ones (`:visibility :hidden`).")}))}

   {:id :per-period-type :severity :warn
    :doc nil
    :scan (fn [schema]
            (let [managed (action-managed-types schema)]
              (for [[nm spec] (user-types schema)
                    :when (and (re-find per-period-re nm)
                               (not (managed nm))
                               (not (:lifecycle spec)))]
                {:entity nm :entity-class :object-types
                 :message (str "Object type " (pr-str nm) " looks like one object per PERIOD"
                               " — a measurement over time, not a thing. Aggregate at read time"
                               " over the objects it summarises, or keep the series on the owning"
                               " object as a `:timeseries` property.")})))}

   {:id :status-lookup-type :severity :warn
    :doc nil
    :scan (fn [schema]
            (let [managed (action-managed-types schema)
                  ends    (link-ends schema)]
              (for [[nm spec] (user-types schema)
                    :when (and (re-find status-name-re nm)
                               (not (managed nm))
                               (not (:lifecycle spec)))
                    :let [keys* (set (map ->name (:match-keys spec)))
                          attrs (remove #(or (attr-hidden? %)
                                             (keys* (->name (:attribute-name %))))
                                        (:attribute-list spec))
                          touch (filter #(or (= nm (:from %)) (= nm (:to %))) ends)]
                    :when (and (<= (count attrs) attribute-priority-limit)
                               (every? (fn [{:keys [from to card]}]
                                         (and (not= from to)
                                              (if (= nm to)
                                                (#{:many-to-one :one-to-one} card)
                                                (= :one-to-many card))))
                                       touch))]
                {:entity nm :entity-class :object-types
                 :message (str "Object type " (pr-str nm) " looks like a status lookup table —"
                               " no Action manages it and it holds no link of its own, it is only "
                               "pointed at."
                               " A state is an attribute of the object that has it (`:constraints"
                               " {:one-of [...]}`, or a `:lifecycle` when Actions move it), not an"
                               " object a person navigates to.")})))}

   {:id :type-name-collision :severity :warn
    :doc nil
    :scan (fn [schema]
            (let [ts (vec (sort-by first (map (fn [[nm spec]] [nm (type-labels nm spec)])
                                              (user-types schema))))]
              (for [i (range (count ts))
                    j (range (inc i) (count ts))
                    :let [[a la] (ts i) [b lb] (ts j)
                          shared (sort (filter la lb))]
                    :when (seq shared)]
                {:entity a :entity-class :object-types
                 :message (str "Object types " (pr-str a) " and " (pr-str b) " both go by "
                               (str/join ", " (map pr-str shared)) " — two objects under one"
                               " name, or one object modelled twice. Merge them, or name each"
                               " for what distinguishes it.")})))}

   {:id :link-side-names :severity :warn
    :doc nil
    :scan (fn [schema]
            (let [users (into #{} (map first) (user-types schema))
                  known (into #{} (keep (fn [[_ s]] (some-> (:object-type-name s) ->name)))
                              (:object-types schema))
                  user? #(or (users %) (not (known %)))]
              (for [{:keys [link from to spec]} (link-ends schema)
                    :when (and (user? from) (user? to))
                    :let [authored? (or (:forward-api-name spec) (:reverse-api-name spec))
                          fwd (->name (or (:forward-api-name spec) link))
                          rev (->name (or (:reverse-api-name spec) link))
                          fnm (some-> (:forward-name spec) str/trim str/lower-case not-empty)
                          rnm (some-> (:reverse-name spec) str/trim str/lower-case not-empty)
                          why (cond
                                (and (not= from to) (= fwd rev))
                                (str "both sides are called " (pr-str fwd) ", so " from "." fwd
                                     " and " to "." rev " read alike. Name each side for the object"
                                     " across it (`:forward-api-name :" to
                                     " :reverse-api-name :<plural of " from ">`).")
                                (and fnm (= fnm rnm))
                                (str ":forward-name and :reverse-name are both " (pr-str fnm)
                                     " — a person cannot tell the directions apart."))]
                    :when why]
                (cond-> {:entity link :entity-class :link-types
                         :message (str "Link type " (pr-str link) ": " why)}
                  (and (not authored?) (not= from to) (= fwd rev)) (assoc :severity :info)))))}

   {:id :lifecycle :severity :error
    :doc nil
    :scan (fn [schema]
            (let [objs (into {}
                             (for [[nm
                                    spec]
                                   (section-entries (:object-types schema) :object-type-name)]
                               [(keyword nm) spec]))
                  acts (into {}
                             (for [[an a] (:actions schema)]
                               [(->kw an)
                                (update a :params
                                        (fn [ps]
                                          (some-> ps
                                                  (update-vals
                                                   #(cond-> %
                                                      (and (map? %) (rid/rid? (:type %)))
                                                      (update :type
                                                              (comp keyword
                                                                    (partial type-ref
                                                                             schema))))))))]))
                  attrs (fn [t] (map (comp ->kw :attribute-name) (:attribute-list (get objs t))))]
              (concat
               (for [{:keys [object-type message]} (lifecycle/problems objs acts attrs)]
                 {:entity (->name object-type) :entity-class :object-types
                  :message (str "Lifecycle of " (pr-str (->name object-type)) ": " message)})
               (for [[an a] acts
                     :let [on (:on a)]
                     :when (and on (seq objs)
                                (not (contains? objs (keyword (type-ref schema on)))))]
                 {:entity (->name an) :entity-class :actions
                  :message (str "Action " (pr-str (->name an)) " is offered `:on "
                                (pr-str (->kw on)) "`, which is not an object type.")}))))}

   {:id :action-rule-op-unknown :severity :error :gate? true :vocab? true
    :doc nil
    :scan (fn [schema vocab]
            (for [[an a] (:actions schema)
                  :let  [problem (spec/unknown-rule-ops-problem vocab a)]
                  :when problem]
              {:entity (->name an) :entity-class :actions
               :message (str "Action " (pr-str (->name an)) ": " problem)}))}

   {:id :action-override-misleading :severity :error :gate? true :vocab? true
    :doc nil
    :scan (fn [schema vocab] (override-findings vocab schema :error))}

   {:id :action-override-redundant :severity :warn :vocab? true
    :doc nil
    :scan (fn [schema vocab] (override-findings vocab schema :warn))}])
