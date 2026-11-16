(ns sutra.access.requests
  (:require [clojure.string :as str]
            [sutra.types.review :as review]))

(set! *warn-on-reflection* true)

(def rules
  [{:when [:always] :by [:owner]}])

(def ^:private admin-only [{:when [:always] :by [[:principal "admin"]] :count 1}])

(def kinds
  {:project-role
   {:needs [:project-rid :role :principal]
    :task  (fn [{:keys [project-rid role principal]}]
             {:task    (str "project-role/" project-rid "/" principal "/" (name role))
              :section "projects"
              :project project-rid
              :what    (str "role " (name role) " for " principal)})}
   :group-membership
   {:needs [:group-id :principal]
    :task  (fn [{:keys [group-id group-project principal]}]
             {:task    (str "group-membership/" group-id "/" principal)
              :section "groups"
              :project group-project
              :what    (str principal " into group " group-id)})}
   :instance-change
   {:needs [:projection :home :schema]
    :task  (fn [{:keys [id projection home schema opts]}]
             {:task    (str "instance-change/" id)
              :section "instance"
              :what    (str "change of "
                            (str/join ", "
                                      (sort (map name
                                                 (distinct
                                                  (concat (keys schema)
                                                          (keys (:strict-projected opts)))))))
                            " on " home " (projection " projection
                            ") — not branched")})}})

(defn problems
  [{:keys [kind] :as request}]
  (if-let [{:keys [needs]} (kinds kind)]
    (not-empty (for [f needs :when (nil? (get request f))]
                 (str "a " (name kind) " request needs " (name f))))
    [(str "unknown request kind " (pr-str kind) " — known: " (pr-str (sort (keys kinds))))]))

(defn tasks
  [{:keys [kind id] :as request}]
  (let [t ((get-in kinds [kind :task]) request)]
    [(assoc t :kind :access :fingerprint (str id)
            :requires (if (:project t)
                        (review/requires rules t {:author (:author request)})
                        admin-only))]))
