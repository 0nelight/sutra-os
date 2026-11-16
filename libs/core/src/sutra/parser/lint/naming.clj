(ns sutra.parser.lint.naming
  (:require [sutra.parser.lint.support :refer [->name section-entries ot-name cryptic-re
                                               code-attr-re title-attr-re temporal-attr-re]]
            [sutra.parser.lint.source :refer [lint-source-config lint-app-source]]
            [sutra.types.rid :as rid]
            [sutra.types.transform-io :as tio]
            [sutra.types.value-types :as vt]))

(set! *warn-on-reflection* true)

(def rules
  [{:id :cryptic-name :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[cls name-field] rid/name-field-of
                  [nm _] (section-entries (get schema cls) name-field)
                  :when (re-find cryptic-re nm)]
              {:entity nm :entity-class cls
               :message (str "Cryptic/placeholder name " (pr-str nm)
                             " — use a descriptive name; incrementing names are hard to read and "
                             "refactor.")}))}

   {:id :entity-without-name :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[cls name-field] rid/name-field-of
                  [k spec] (get schema cls)
                  :when (and (rid/rid? k) (nil? (get spec name-field)))]
              {:entity (->name k) :entity-class cls
               :message (str "Entry " (pr-str (->name k)) " under " (pr-str cls)
                             " carries no " (pr-str name-field)
                             " — a RID-keyed entry whose name field is missing has no name"
                             " at all. Every rule that reads entities BY NAME passes over"
                             " it, and so does every reference that resolves by name; the"
                             " RID is all that is left of it.")}))}

   {:id :missing-identity :severity :warn
    :doc nil
    :scan (fn [schema]
            (for [[nm spec] (section-entries (:object-types schema) :object-type-name)
                  :when (and (empty? (:match-keys spec)) (not (:resolve-entity spec))
                             (not (:primary-key spec)))]
              {:entity nm :entity-class :object-types
               :message (str "Object type " (pr-str nm)
                             " has no match-keys / entity resolution — its primary key must be "
                             "DETERMINISTIC: a key that differs on the next build is a different "
                             "object, and the edits and links of the old one are left behind.")}))}

   {:id :primary-key-property :severity :error
    :doc nil
    :scan (fn [schema]
            (let [
                  produced (reduce (fn [m o]
                                     (let [n (some->> (:object-type o) (ot-name schema))]
                                       (cond-> m
                                         (and n (not= :supplement (:role o)) (:primary-key o)
                                              (not (contains? m n)))
                                         (assoc n (:primary-key o)))))
                                   {}
                                   (mapcat (comp tio/outputs-of val) (:transforms schema)))]
              (for [[nm spec] (section-entries (:object-types schema) :object-type-name)
                    :let [pk    (some-> (:primary-key spec) ->name)
                          attrs (set (map (comp ->name :attribute-name) (:attribute-list spec)))
                          data  (some-> (or (get-in spec [:backing :primary-key]) (produced nm))
                                        ->name)
                          msg   (cond
                                  (nil? pk) nil
                                  (not (attrs pk))
                                  (str "Object type " (pr-str nm) " names :primary-key "
                                       (pr-str pk) ", which is none of its properties — the"
                                       " primary key is a property of the type.")
                                  (and data (not= data pk))
                                  (str "Object type " (pr-str nm) " names :primary-key "
                                       (pr-str pk) " while its data is keyed on " (pr-str data)
                                       " — two keys for one object. Changing the key is an"
                                       " identity migration: change it where the rows are"
                                       " minted, and keep both equal."))]
                    :when msg]
                {:entity nm :entity-class :object-types :message msg})))}

   {:id :missing-primary-key :severity :error :gate? true :scope :change
    :doc nil
    :scan (fn [schema]
            (let [produced (into #{}
                                 (for [[_ t] (:transforms schema)
                                       o (tio/outputs-of t)
                                       :when (and (:primary-key o)
                                                  (not= :supplement (:role o)))
                                       :let [ot (:object-type o)]
                                       :when ot]
                                   (ot-name schema ot)))]
              (for [[nm spec] (section-entries (:object-types schema) :object-type-name)
                    :when (not (or (:primary-key spec)
                                   (get-in spec [:backing :primary-key])
                                   (produced nm)))]
                {:entity nm :entity-class :object-types
                 :message (str "Object type " (pr-str nm) " has no primary key. Every"
                               " object type requires one, also one only actions fill — each"
                               " object an action creates needs a unique value. Declare"
                               " :primary-key on the type (e.g. the id property the action"
                               " prefills with :uuid).")})))}

   {:id :numeric-cast-suspect :severity :info
    :doc nil
    :scan (fn [schema]
            (for [[nm spec] (section-entries (:object-types schema) :object-type-name)
                  a (:attribute-list spec)
                  :let [an (->name (:attribute-name a))]
                  :when (and an
                             (vt/numeric? (:attribute-type a))
                             (re-find code-attr-re an))]
              {:entity (str nm "." an) :entity-class :object-types
               :message (str "Attribute " (pr-str an) " on " (pr-str nm)
                             " is a numeric type but looks like a code/identifier — "
                             "only cast to numeric if you'd do arithmetic on it; otherwise keep it "
                             "a string.")}))}

   {:id :temporal-as-string :severity :info
    :doc nil
    :scan (fn [schema]
            (for [[nm spec] (section-entries (:object-types schema) :object-type-name)
                  a (:attribute-list spec)
                  :let [an (->name (:attribute-name a))
                        ty (:attribute-type a)]
                  :when (and an
                             (or (= :string ty) (= :text ty))
                             (re-find temporal-attr-re an)
                             (not (re-find code-attr-re an)))]
              {:entity (str nm "." an) :entity-class :object-types
               :message (str "Attribute " (pr-str an) " on " (pr-str nm)
                             " is named like a date/time but is declared "
                             (pr-str ty) " — so it compares and sorts LEXICALLY,"
                             " cannot be bucketed by period, and never gets the"
                             " canonical UTC cast a declared temporal column does."
                             " Cast it in the producing transform and declare it"
                             " `:date` (date-only) or `:timestamp` (an instant).")}))}

   {:id :no-title :severity :info
    :doc nil
    :scan (fn [schema]
            (for [[nm spec] (section-entries (:object-types schema) :object-type-name)
                  :when (nil? (:title-key spec))
                  :let [attrs     (keep #(->name (:attribute-name %)) (:attribute-list spec))
                        candidate (first (filter #(re-find title-attr-re %) attrs))]
                  :when (seq attrs)]
              {:entity nm :entity-class :object-types
               :message (if candidate
                          (str "Object type " (pr-str nm) " has no :title key declared, but "
                               (pr-str candidate) " looks like one — declare `:title :" candidate
                               "` so pickers, search results and object references render the "
                               "instance instead of its primary key.")
                          (str "Object type " (pr-str nm)
                               " has no :title key and no obvious title property (name/title/"
                               "label/…) — without one, every instance displays as its primary key."))}))}

   {:id :source-config :severity :warn :vocab? true
    :doc nil
    :scan (fn [schema vocab]
            (lint-source-config
             (:source-types vocab)
             (for [[_rid s] (:sources schema)]
               {:name (or (:source-name s) (:name s)) :type (:type s) :config (:config s)})))}

   {:id :app-source :severity :error
    :doc nil
    :scan (fn [schema]
            (lint-app-source
             (for [[_rid a] (:applications schema)]
               {:name (or (:application-name a) (:name a)) :source (:source a)})))}])
