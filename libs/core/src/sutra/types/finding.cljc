(ns sutra.types.finding
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]))

(def severities
  {:error 0 :warn 1 :info 2})

(def Severity (into [:enum] (sort-by severities (keys severities))))

(def Finding
  [:map
   [:rule :keyword]
   [:severity Severity]
   [:entity-class {:optional true} :keyword]
   [:entity {:optional true} [:or :keyword :string :symbol]]
   [:message {:optional true} :string]
   [:where {:optional true} [:sequential :any]]])

(defn- ranked!
  [rule severity]
  (when-not (contains? severities severity)
    (anomaly/raise! :fault (str "no finding severity " (pr-str severity))
                    {:rule rule :severity severity :severities (keys severities)}))
  severity)

(defn finding
  ([rule severity entity-class entity] (finding rule severity entity-class entity nil))
  ([rule severity entity-class entity more]
   (cond-> (assoc more :rule rule :severity (ranked! rule severity))
     (some? entity-class) (assoc :entity-class entity-class)
     (some? entity)       (assoc :entity entity))))

(defn ranked
  [f default]
  (assoc f :severity (ranked! (:rule f) (or (:severity f) default))))

(defn blocking?
  [f]
  (= :error (:severity f)))

(defn worst-first [findings]
  (vec (sort-by (fn [f] [(severities (:severity f) 9) (str (:entity f))]) findings)))

(defn findings->refusal
  ([findings] (findings->refusal findings nil))
  ([findings {:keys [code data params]}]
   (when-let [bs (not-empty (worst-first (filter blocking? findings)))]
     (let [f (first bs)]
       (refusal/refusal (or code :refusal.finding/blocking)
                        (assoc data :problems bs)
                        (merge {:n     (count bs)
                                :first (or (:message f)
                                           (str (some-> (:rule f) name) " "
                                                (some-> (:entity f) name)))}
                               (if (fn? params) (params bs) params)))))))
