(ns sutra.cli.verbs.access
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.out :refer [emit! mark paint render say!]]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra.wire.api :as api]))

(defmethod step! :access-role [ctx [_ {user :user role :set}]]
  (emit! ctx (req! (str "access role " user)
                   (if role (api/post! :access-role {:user user} {:role role})
                       (api/get! :access-role {:user user}))))
  nil)

(defn- because-lines
  [e depth]
  (when e
    (cons (str (apply str (repeat depth "  "))
               (if-let [r (:rule e)] (str (name r) " → ") "fact ")
               (pr-str (:fact e)))
          (mapcat #(because-lines % (inc depth)) (:from e)))))

(defmethod step! :access-request [ctx [_ {:keys [kind target role for justification]}]]
  (let [k    (case (str kind) ("role" "project-role") "project-role"
                   ("group" "group-membership") "group-membership" (str kind))
        body (cond-> {:kind k}
               (= k "project-role")     (assoc :project target :role role)
               (= k "group-membership") (assoc :group target)
               for                      (assoc :for for)
               justification            (assoc :justification justification))]
    (emit! ctx (req! (str "access request " k) (api/post! :access-requests body)))
    nil))

(defmethod step! :access-decide [ctx [_ {:keys [id reject? note]}]]
  (emit! ctx (req! (str "access decide " id)
                   (api/post! (if reject? :access-request-reject :access-request-approve) {:id id}
                              (cond-> {} note (assoc :note note)))))
  nil)

(defmethod step! :access-explain [ctx [_ {:keys [user op project]}]]
  (let [r (req! (str "access explain " user)
                (api/get! :access-explain {:user user :op op :project project}))]
    (emit! ctx r ::explain)
    nil))

(defmethod render ::explain [_ r]
  [(str (if (:allow? r) (paint :green "✓ allowed") (paint :red "✗ not allowed"))
        "  " (:username r) " · " (:op r) " · " (:project r))
   (if (:allow? r)
     (because-lines (:because r) 1)
     (paint :dim
            "  no rule derives it — no grant, group or platform role gives this operation here"))])

(defmethod step! :access [ctx [_ {:keys [user project]}]]
  (let [{:keys [principals roles projects markings matrix]}
        (req! "access" (api/get! :access))
        dens    (fn [s] (str/replace (str s) #"^:" ""))
        by-user (into {} (map (juxt :username identity)) principals)
        by-proj (into {} (map (fn [pr] [(dens (:name pr)) pr])) projects)
        uid->name (into {} (map (juxt :user-id :username)) principals)
        who     (fn [pid] (get uid->name pid pid))
        prow    (fn [p]
                  {:id          (:username p)
                   :role        (or (:role p) "—")
                   :auth-source (str (:auth-source p))
                   :clearances  (cond (:admin p)            "superuser (all markings)"
                                      (seq (:clearances p)) (str/join "," (:clearances p))
                                      :else                 "—")})]
    (cond
      user
      (if-let [p (get by-user user)]
        (let [m (some #(when (= (:username %) user) %) matrix)]
          (emit! ctx (assoc p :grants (:grants m)))
          {:kind :objects :rows [(prow p)]})
        (do (say! ctx (mark :yellow "∅" (i18n/t :cli/no-principal {:name (pr-str user)}))) nil))

      project
      (if-let [pr (get by-proj (dens project))]
        (do
          (emit! ctx pr)
          {:kind :objects :rows (mapv (fn [g]
                                        {:id   (:principal g)
                                         :role (or (:role g) "—")
                                         :who  (who (:principal g))})
                                      (:grants pr))})
        (do (say! ctx (mark :yellow "∅" (i18n/t :cli/no-project {:name (pr-str project)}))) nil))

      :else
      (do (emit! ctx {:principals principals :roles roles
                      :projects projects :markings markings :matrix matrix})
          {:kind :objects :rows (mapv prow principals)}))))

