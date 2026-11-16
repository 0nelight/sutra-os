(ns sutra.cli.verbs.actions
  (:require [sutra.cli.dispatch :refer [resolve-refs step!]]
            [sutra.cli.errors :as errors :refer [req!]]
            [sutra.cli.live :refer [schema-sections]]
            [sutra.cli.out :refer [emit! mark paint prompt-yes? render say! show-lines]]
            [sutra.i18n.core :as i18n]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.types.submission :as csub]
            [sutra.wire.api :as api]
            [sutra.cli.view :refer
             [cell]]))

(defn- action-of
  [body a]
  (let [a (keyword a)]
    (some (fn [[k spec]] (when (or (= k a) (= (some-> (:action-name spec) keyword) a)) spec))
          (:actions body))))

(defn- action-arg-hint
  [body action-nm]
  (let [declared (:params (action-of body action-nm))
        params   (seq (map clojure.core/name (keys declared)))
        required (set (map clojure.core/name
                           (keys (filter (comp csub/client-gated? val) declared))))
        crud     (some-> (re-matches #"(?:create|update|delete)-(.+)" action-nm) second)]
    (cond
      params {:params (vec (sort params)) :required required}
      crud   {:crud crud}
      :else  nil)))

(defn- plan-lines
  [plan]
  (if (seq plan)
    (show-lines (mapv #(update-vals % cell) plan))
    [(str "  " (i18n/t :cli/no-changes))]))

(defmethod render ::plan [_ {:keys [plan action]}]
  [(plan-lines plan)
   (mark :dim "•" (i18n/t :cli/plan-only {:name (paint :bold action)}))])

(defn pin-generated-params
  [args evaluation]
  (let [uuid? (fn [[_ d]] (= "uuid" (some-> (or (:prefill d) (get d "prefill")) clojure.core/name)))
        ps    (into #{} (comp (filter uuid?) (map (comp keyword clojure.core/name key)))
                    (:params evaluation))
        vals  (into {} (keep (fn [[k v]] (let [k (keyword (clojure.core/name k))]
                                           (when (ps k) [k v]))))
                    (:values evaluation))
        fill  (fn [m] (merge vals m))]
    (cond
      (empty? vals)         args
      (map? (:arg0 args))   (update args :arg0 fill)
      :else                 (fill args))))

(defn pin-minted-id
  [args preview]
  (if-let [id (and (not (:id args)) (:id preview))]
    (assoc args :id (str id))
    args))

(def ^:private batch-size
  20)

(defn batch-requests
  [text]
  (let [t (str/trim (str text))]
    (if (str/starts-with? t "[")
      (mapv #(update-keys % keyword) (edn/read-string t))
      (into []
            (comp (map str/trim) (remove #(or (str/blank? %) (str/starts-with? % ";")))
                  (map (fn [line]
                         (into {} (map (fn [kv]
                                         (let [[k v] (str/split kv #"=" 2)]
                                           [(keyword k) v])))
                               (str/split line #"\s+")))))
            (str/split-lines t)))))

(defn- run-batch!
  [ctx action file plan?]
  (let [nm       (clojure.core/name action)
        ps       (:params (errors/optional "action parameters"
                                          #(action-of (schema-sections :actions) action)))
        requests (cond->> (batch-requests (slurp file))
                   ps (mapv #(csub/decode-params-text ps %)))
        packets  (partition-all batch-size requests)]
    (say! ctx (mark :dim "•" (str nm ": " (count requests) " requests in "
                                  (count packets) " packets of at most " batch-size)))
    (cond
      plan?
      (emit! ctx {:action nm :batch (count requests) :first (first requests)} ::plan)

      (or (:yes? ctx)
          (prompt-yes? ctx (i18n/t :cli/apply-these-changes {:name (paint :bold nm)})))
      (let [done (reduce (fn [n packet]
                           (req! (str "batch " nm " " (inc n) "–" (+ n (count packet)))
                                 (api/post! :actions-batch {:publish true}
                                            {:action action :requests (vec packet)}))
                           (+ n (count packet)))
                         0 packets)]
        (emit! ctx {:dispatched nm :batch done}))

      :else
      (say! ctx (mark :yellow "⏸" (str nm ": nothing applied"))))))

(declare ^:private step-one!)

(defmethod step! :action
  [ctx [_ {:keys [name args stage? plan? batch]}]]
  (if batch
    (run-batch! ctx (keyword name) batch plan?)
    (step-one! ctx name args stage? plan?)))

(defn- step-one!
  [ctx name args stage? plan?]
  (let [action (keyword name)
        nm     (clojure.core/name action)
        args   (resolve-refs ctx (or args {}))
        args   (if-let [ps (and (seq args)
                                (:params (errors/optional
                                          "action parameters"
                                          #(action-of (schema-sections :actions) action))))]
                 (let [decode #(csub/decode-params-text ps %)]
                   (if (map? (:arg0 args))
                     (update args :arg0 decode)
                     (decode args)))
                 args)]
    (when (empty? args)
      (when-let [hint (errors/optional "action arguments"
                                       #(action-arg-hint (schema-sections :actions) nm))]
        (cond
          (:params hint)
          (say! ctx (mark :dim "•"
                          (str (i18n/t :cli/action-accepts
                                       {:name (paint :bold nm)
                                        :params (str/join " " (map (fn [p]
                                                                     (str p "=…"
                                                                          (when ((:required hint) p)
                                                                            "*")))
                                                                   (:params hint)))})
                               (when (seq (:required hint))
                                 (str "   (* " (i18n/t :cli/required) ")")))))
          (:crud hint)
          (say! ctx (mark :dim "•" (i18n/t :cli/action-needs-key
                                         {:name (paint :bold nm)
                                          :form (paint :bold "<key>=<value>")
                                          :typ  (:crud hint)}))))))
    (cond
      stage?
      (do (req! (str "stage " nm) (api/mutate action args {}))
          (emit! ctx {:staged nm}))

      (and (:yes? ctx) (not plan?))
      (let [res (req! (str "action " nm)
                      (api/mutate action args {:publish? true :return-edits? true}))]
        (when-let [edits (seq (:edits (:edits res)))]
          (run! (partial say! ctx) (plan-lines (vec edits))))
        (emit! ctx (cond-> {:dispatched nm}
                     (:action-id res) (assoc :action-id (:action-id res)))))

      :else
      (let [args (pin-generated-params
                  args
                  (try (req! "params"
                             (api/post! :action-params nil
                                        (cond-> {:action action :args (or (:arg0 args) args)}
                                          (seq nil) (assoc :search nil))))
                       (catch Exception _ nil)))
            pre  (req! (str "plan " nm) (api/mutate action args {:dry-run? true}))
            args (pin-minted-id args pre)
            plan (vec (remove nil? (:plan pre)))]
        (when-not plan? (run! (partial say! ctx) (plan-lines plan)))
        (cond
          plan?
          (emit! ctx {:action nm :plan plan} ::plan)

          (or (:yes? ctx)
              (prompt-yes? ctx (i18n/t :cli/apply-these-changes {:name (paint :bold nm)})))
          (let [res (req! (str "action " nm) (api/mutate action args {:publish? true}))]
            (emit! ctx (cond-> {:dispatched nm}
                         (:action-id res) (assoc :action-id (:action-id res)))))

          :else
          (do (say! ctx (mark :yellow "⏸"
                              (i18n/t :cli/declined-not-applied {:name (paint :bold nm)})))
              :abort))))))
