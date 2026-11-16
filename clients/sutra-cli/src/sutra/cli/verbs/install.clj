(ns sutra.cli.verbs.install
  (:require [sutra.cli.auth :refer [gen-password load-creds save-creds! seal-schema-sources]]
            [sutra.cli.dispatch :refer [ref! step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.live :refer [live-schema rider-kinds]]
            [sutra.cli.local :refer [merge-view-files resolve-code-files]]
            [sutra.cli.out :refer [emit! mark paint render show-lines warn!]]
            [sutra.i18n.core :as i18n]
            [sutra.wire.api :as api]))

(defn- ent-read-ent             [& a]
  (apply @(requiring-resolve 'sutra.parser.ent-file/read-ent) a))
(defn- lint-source-config
  [sources]
  (@(requiring-resolve 'sutra.parser.lint.source/lint-source-config)
   @(requiring-resolve 'sutra.parser.spec/default-source-types) sources))
(defn- lint-app-source          [& a]
  (apply @(requiring-resolve 'sutra.parser.lint.source/lint-app-source) a))

(defn- upsert-riders!
  [kind rows]
  (let [{:keys [noun call target?]} (rider-kinds kind)]
    (mapv (fn [r]
            (req! (str noun " " (:name r)) (call r))
            (cond-> {:name (:name r)} (and target? (:target r)) (assoc :target (:target r))))
          rows)))

(defmethod step! :automations [ctx [_ autos]]
  (emit! ctx (upsert-riders! :automations autos)))

(defmethod step! :schedules [ctx [_ scheds]]
  (emit! ctx (upsert-riders! :schedules scheds)))

(defmethod step! :health-checks [ctx [_ checks]]
  (emit! ctx (upsert-riders! :health-checks checks)))

(defmethod step! :install [ctx [_ {:keys [schema preset auto-ds strict?]}]]
  (let [ent    (when preset (ent-read-ent preset))
        schema (seal-schema-sources ctx (or schema (-> ent :schema
                                                       (resolve-code-files preset)
                                                       (merge-view-files preset (live-schema)))))
        _pre  (let [fs (concat (lint-source-config (:sources schema))
                               (lint-app-source
                                (for [[a-kw a] (:applications schema)]
                                  {:name (or (:name a) a-kw) :source (:source a)})))]
                (doseq [{:keys [severity entity message]} fs]
                  (let [err? (= :error (keyword severity))]
                    (warn! (mark (if err? :red :yellow) (if err? "✗" "⚠")
                                 (str (paint :bold (str entity)) "  " message)))))
                (when-let [errs (seq (filter #(= :error (keyword (:severity %))) fs))]
                  (throw (ex-info (i18n/t :cli/install-aborted {:n (count errs)})
                                  {:findings (vec errs)}))))
        tally (req! "install"
                    (api/post! :install (cond-> {:schema schema}
                                          auto-ds (assoc :auto-ds auto-ds)
                                          strict? (assoc :strict? true))))]
    (emit! ctx (reduce (fn [m k] (if-let [rows (seq (get ent k))]
                                   (assoc m k (upsert-riders! k rows))
                                   m))
                       (cond-> {:staged tally} preset (assoc :preset preset))
                       [:automations :schedules :health-checks]))))

(defn commit-problems
  [findings problems]
  (let [{inherited true own false} (group-by (comp boolean :inherited?) findings)]
    (cond-> {:problems (vec (or (seq own) (when-not (seq inherited) problems)))}
      (seq inherited) (assoc :inherited-from-trunk (count inherited)))))

(defmethod step! :commit [ctx [_ _]]
  (let [{:keys [valid? findings problems]}
        (req! "schema integrity check" (api/get! :schema-problems))
        _ (req! "commit" (api/post! :staging))]
    (emit! ctx (cond-> {:committed true}
                 (not valid?) (merge (commit-problems findings problems)))
           ::commit)))

(defmethod render ::commit [_ {:keys [problems] :as v}]
  (if (contains? v :problems)
    [(mark :yellow "⚠" (i18n/t :cli/schema-problems-reported))
     (show-lines problems)
     (show-lines (dissoc v :problems))]
    (show-lines v)))

(defmethod step! :datasources [ctx [_ specs]]
  (emit! ctx {:datasources (mapv (fn [[k spec]]
                                   (req! (str "datasource " k)
                                         (api/post! :datasources (assoc spec :name (name k))))
                                   (name k))
                                 specs)}))

(defmethod step! :principals [ctx [_ {:keys [users]}]]
  (let [made
        (doall
         (for [{:keys [ref username password user-id external auth-source]} users]
           (let [federated? (some? external)
                 pw  (when-not federated?
                       (if (= :auto password) (gen-password) password))
                 uid (or user-id (when ref (ref! ctx ref)))]
             (req! (str "register " username)
                   (api/post! :register
                              (cond-> {:username username :branch "main" :user-id uid}
                                pw         (assoc :password pw)
                                federated? (assoc :external-subject external
                                                  :auth-source (or auth-source :spnego)))))
             (when-not federated?
               (let [path  (:creds-path ctx)
                     creds (load-creds path)]
                 (save-creds! path
                              (assoc-in creds [:users (keyword (or ref username))]
                                        {:username username :password pw :user-id uid}))))
             (cond-> {:id uid :label (str "login " username) :principal username}
               federated? (assoc :external external)))))]
    (emit! ctx (mapv #(dissoc % :label) made))
    {:kind :objects :rows (mapv #(select-keys % [:id :label]) made)}))

(defmethod step! :branch [ctx [_ {:keys [name from]}]]
  (let [child (clojure.core/name name)]
    (req! (str "branch " child)
          (api/post! :branches {:parent (or from "main") :name child}))
    (emit! ctx {:branch child :from (or from "main")})
    {:kind :branch :branch child}))

(defmethod step! :branch-pipeline [ctx [_ {:keys [seed label from]}]]
  (let [sd  (clojure.core/name seed)
        bn  (str "pipeline/" sd (when label (str "/" (clojure.core/name label))))
        par (or from "main")]
    (req! (str "branch " bn)   (api/post! :branches {:parent par :name bn}))
    (emit! ctx {:seed sd :branch bn :from par})
    {:kind :branch :branch bn}))
