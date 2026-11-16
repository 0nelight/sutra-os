(ns sutra.authoring.install.findings.apps
  (:require [sutra.capps.catalog :as cat]
            [sutra.capps.layout-refs :as layout-refs]
            [sutra.capps.refs :as refs]
            [sutra.types.set-query :as sq]
            [sutra.catalog.actions :as actions]
            [sutra.catalog.functions :as functions]
            [sutra.kit.coerce :refer [->name-kw]]
            [sutra.types.shape :as tshape]
            [clojure.string :as str]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.findings :as fnd]
            [sutra.authoring.install.live :as live]
            [sutra.types.rid :as rid]
            [sutra.types.template :as tmpl]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(def template-search-around
  {:find (fn [{:keys [text] :as d}]
           (let [near (diff/links-near d)]
             (for [[t-kw {:keys [parameters]}] (:templates text)
                   [pname p] parameters
                   :when (:search-around p)
                   :let [msg (tmpl/search-around-problem parameters pname near)]
                   :when msg]
               {:rule :template-search-around :entity-class :template :entity t-kw
                ::fnd/ctx {:text msg}})))
   :says {:template-search-around
          (fn [{t-kw :entity {:keys [text]} ::fnd/ctx}]
            (str "template " (name t-kw) " — " text))}})

(defn- unresolved-args
  [spec args fills]
  (let [inputs (into {} (tio/call-inputs spec))]
    (vec (for [[k v] (sort-by (comp str key) args)
               :let [alias (->name-kw k)]
               :when (not (tio/takes-map? (get inputs alias)))
               form (cat/unresolved-arg-forms v fills)]
           {:input alias :form form}))))

(defn- function-call-problems
  [type-nm types fns transforms {:keys [function args fills]}]
  (let [spec (:spec (functions/function-by-name {:transforms transforms} function))]
    (cond
      (not (fns function)) [{:rule :app-function-not-listed}]
      (nil? spec)          [{:rule :app-function-unknown}]
      :else
      (let [stray (vec (sort (remove types (keep type-nm (tio/object-type-inputs spec)))))
            unb   (tio/unbound-scalars spec (into #{} (keep ->name-kw) (keys args)))
            raw   (unresolved-args spec args fills)]
        (concat
         (when (seq stray) [{:rule :app-function-type-outside :types stray}])
         (when (seq unb)   [{:rule :app-function-scalar-unbound :unbound unb}])
         (when (seq raw)   [{:rule :app-function-arg-unresolved :unresolved raw}]))))))

(defn- app-call-head
  [{:keys [entity var path function]}]
  (str "App " (pr-str entity) ", variable " (pr-str var) " (" path ") calls "
       (pr-str function)))

(def app-functions
  {:find (fn [{:keys [text current after] :as d}]
           (let [type-nm (fn [x] (if (rid/rid? x)
                                   (get-in current [:object-types x :object-type-name])
                                   (->name-kw x)))
                 fs      (into #{} (comp (mapcat (comp cat/function-vars val))
                                         (keep (comp ->name-kw :function)))
                               (:applications text))
                 tx      (:transforms (diff/sub-world after {:transforms {}}
                                                      (diff/fn-nodes d fs)))]
             (for [[app-nm app] (:applications text)
                   :let [types (into #{} (keep type-nm) (cat/scope-tokens app :type))
                         fns   (into #{} (keep ->name-kw) (cat/scope-tokens app :function))]
                   {:keys [var function path] :as call} (cat/function-vars app)
                   f (function-call-problems type-nm types fns tx call)]
               (merge {:severity :error :entity-class :application :entity app-nm
                       :function function :var var
                       :path (str (name app-nm) " > " (apply fnd/where path))}
                      f))))
   :says
   {:app-function-not-listed
    (fn [{:keys [function] :as f}]
      (str (app-call-head f) ", which the app does not list in :functions — the app"
           " door answers 404 \"no function\" when it opens. Add "
           (pr-str function) " to the app's :functions."))

    :app-function-unknown
    (fn [f]
      (str (app-call-head f) ", which is no query Function (none is declared or"
           " installed by that name) — the call fails when the app opens."))

    :app-function-type-outside
    (fn [{:keys [types] :as f}]
      (str (app-call-head f) ", which reads " (str/join ", " (map pr-str types))
           " — not in the app's :object-types or :aggregate-types, so the"
           " app door refuses the call with 404 \"function reads a type not"
           " in this application\". Name the type(s) in :aggregate-types"
           " (or :object-types, if the app shows their rows)."))

    :app-function-scalar-unbound
    (fn [{:keys [unbound function] :as f}]
      (str (app-call-head f) ", which declares the :scalar input(s) "
           (str/join ", " (map pr-str unbound)) " that the variable's :args"
           " do not bind — the call is refused when the app opens. Bind"
           " each in :args, e.g. {:function " (pr-str function)
           " :args {" (pr-str (first unbound)) " {:val …}}}."))

    :app-function-arg-unresolved
    (fn [{:keys [unresolved] :as f}]
      (str (app-call-head f) " and binds "
           (str/join ", " (map (fn [{:keys [input form]}]
                                 (str (pr-str input) " to " (pr-str form)))
                               unresolved))
           " — nothing resolves that form here, so the Function would receive the map itself"
           " and find nothing. :args take a literal or a reference to a variable: declare"
           " one, e.g. :my-key {:object-property :some-prop :of :object}, and bind"
           " {:val :my-key}. {:item p} is filled only inside a :loop body, and inside a"
           " nested :vars of an object view."))}})

(def app-binds-unknown-type
  {:find (fn [{:keys [text current] :as d}]
           (let [known (into #{} (map keyword)
                             (concat (keys (:objects text)) (keys (:interfaces text))
                                     (keep :object-type-name (vals (:object-types current)))
                                     (keep :interface-name (vals (:interfaces current)))))]
             (for [[entity args] (diff/staged d :application)
                   t (concat (get-in args [:layout :object-types])
                             (get-in args [:layout :aggregate-types]))
                   :when (and (not (rid/rid? (str t))) (not (known (keyword t))))]
               {:rule :app-binds-unknown-type :entity-class :application :entity entity
                ::fnd/ctx {:type t}})))
   :says {:app-binds-unknown-type
          (fn [{:keys [entity] {t :type} ::fnd/ctx}]
            (str "App " (pr-str (name entity)) " binds " (pr-str t)
                 ", which is neither an object type nor an interface"))}})

(defn- by-name-of
  [d kk f]
  (let [m (diff/side-by-name-kw d :after kk)]
    (live/mapped m (fn [_ e] (f e))
                 (delay (update-vals (live/whole m) f)))))

(defn- world-fields
  [d]
  (let [ifaces (diff/side-by-name-kw d :after :interfaces)
        own-of (fn [i] (map :attribute-name
                            (:property-list (live/lookup ifaces (->name-kw i)))))]
    (by-name-of d :object-types
                (fn [o] (into #{:id :rid} (keep ->name-kw)
                              (concat (map :attribute-name (:attribute-list o))
                                      (mapcat own-of (:implements o))))))))

(defn- world-actions
  [d]
  (by-name-of d :actions (fn [spec] (into #{} (keep ->name-kw) (keys (:params spec))))))

(defn- row-of
  [spec]
  (when (and (tshape/object-view? spec) (not= "panel-set" (some-> (:form-factor spec) name)))
    (some-> (:object-type spec) ->name-kw)))

(defn layout-reference-findings
  [{:keys [before] :as d}]
  (let [world  {:actions (partial live/lookup (world-actions d))
                :types   (partial live/lookup (world-fields d))}
        ctx    (fn [spec] (assoc world :item-type (row-of spec)))
        judged (fn [spec] (layout-refs/problems (:layout spec) (ctx spec)))
        same   (juxt :rule :action :param :field)]
    (for [[entity args] (diff/staged d :application)
          :let [old-spec (live/lookup (:applications before) entity)
                held     (into #{} (map same) (when old-spec (judged old-spec)))]
          p (distinct (judged (merge old-spec args)))]
      {:rule :layout-reference :entity-class :application :entity entity
       :severity (if (held (same p)) :warn :error)
       :message (str "App " (pr-str (name entity)) " ("
                     (apply fnd/where (:path p)) "): " (layout-refs/message p))})))

(def app-param-bindings
  {:find (fn [d]
           (for [[entity args] (diff/staged d :application)
                 :let [layout (:layout args)]
                 :when (map? layout)
                 p (cat/binding-kind-problems layout)]
             {:rule :app-param-binding :severity :warn :entity-class :application
              :entity entity
              :message (str "App " (pr-str (name entity)) " (" (apply fnd/where (:path p))
                            "): " (:error p) " — the widget reads the value instead of the"
                            " variable, writes nowhere, or finds no set")}))})

(defn- type-name-of [world x]
  (->name-kw (or (:object-type-name (live/lookup (:object-types world) x)) (rid/->name x))))

(defn- linked-pair? [links-near world a b]
  (some (fn [{:keys [from to]}]
          (let [f (type-name-of world from) t (type-name-of world to)]
            (or (and (= f a) (= t b)) (and (= f b) (= t a)))))
        (vals (links-near a))))

(defn- hand-hops
  [spec links-near world]
  (let [layout (:layout spec)
        root   (row-of spec)]
    (when (and root (map? layout))
      (let [specs (update-keys (sq/layout-specs layout) ->name-kw)]
        (for [[nm s] specs
              :let [[t of] (refs/hand-made-hop specs s)]
              :when t
              :let [from (if (= :object of) root (some-> (get-in specs [of :linked]) ->name-kw))]
              :when (and from (linked-pair? links-near world from t))]
          [nm t of])))))

(def app-hand-made-hops
  {:find (fn [{:keys [before after] :as d}]
           (let [near (diff/links-near d)]
             (for [[entity args] (diff/staged d :application)
                   :let [old  (live/lookup (:applications before) entity)
                         held (set (hand-hops old near before))]
                   [nm t of :as h] (distinct (hand-hops (merge old args) near after))]
               {:rule :hand-made-hop :entity-class :application :entity entity
                :severity (if (held h) :warn :error)
                :message (str "App " (pr-str (name entity)) ": " (name nm) " finds "
                              (name t) " by a key plucked off " (name of) " — a link followed"
                              " by hand. link traversal instead: {:linked " (name t)
                              (when-not (= :object of) (str " :of " (name of))) "}")})))})

(def app-lint
  {:find (fn [d]
           (concat
            (for [[entity args] (diff/staged d :application)
                  f (actions/app-lint-findings entity args #{:var-shadows-type})]
              (assoc (select-keys f [:rule :message]) :entity-class :application :entity entity))
            (layout-reference-findings d)))})

(def view-in-repository
  {:find (fn [{:keys [text]}]
           (for [[nm spec] (:applications text)
                 :when (and (tshape/object-view? spec) (some? (:repository spec)))]
             {:rule :view-in-repository :entity-class :application :entity nm
              ::fnd/ctx (select-keys spec [:form-factor :repository :object-type])}))
   :says {:view-in-repository
          (fn [{nm :entity {:keys [form-factor repository object-type]} ::fnd/ctx}]
            (str (name nm) " is an Object View (:form-factor "
                 (pr-str form-factor) ") and carries :repository "
                 (pr-str repository) ". A view belongs to its object"
                 " type, not to a code repository: it is emitted into the"
                 " ONTOLOGY tree as object-views/" (some-> object-type name)
                 ".entv, which needs no membership. Drop the :repository."
                 " A standalone app is the other case and does belong in one."))}})
