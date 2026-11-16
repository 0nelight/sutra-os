(ns sutra.apps.http.scenarios
  (:require [clojure.string :as str]
            [sutra.http.responses :refer [negotiated]]
            [sutra.store.access.scope :as ctx]
            [sutra.actions.api :as actions]
            [sutra.governance.api :as governance]
            [sutra.scenarios.api :as scenarios]
            [sutra.store.seat :as seat]
            [sutra.apps.http.session :refer [scenario-app-prefix scenario-branch with-app]]))

(set! *warn-on-reflection* true)

(defn- b64url-encode [s]
  (-> (.encodeToString (java.util.Base64/getUrlEncoder) (.getBytes (str s) "UTF-8"))
      (str/replace "=" "")))

(defn- b64url-decode [^String s]
  (String. (.decode (java.util.Base64/getUrlDecoder) s) "UTF-8"))

(defn- scenario-seg
  [uuid name]
  (str uuid "." (b64url-encode name)))

(defn- scenario-seg->name
  [seg]
  (when-let [i (str/index-of seg ".")]
    (try (b64url-decode (subs seg (inc i))) (catch IllegalArgumentException _ nil))))

(defn my-application-scenarios
  [req]
  (with-app req {}
    (fn [{:keys [rid store]}]
      (let [pfx (scenario-app-prefix rid)]
        (negotiated req
                    {:scenarios
                     (->> (governance/branch-parents store)
                          (keep (fn [{:keys [name]}]
                                  (when-let [seg (and name (str/starts-with? name pfx)
                                                      (subs name (count pfx)))]
                                    (when-not (str/includes? seg "/")
                                      {:scenario seg
                                       :name     (or (scenario-seg->name seg) seg)}))))
                          vec)})))))

(defn my-application-scenario-create
  [req]
  (with-app req {}
    (fn [{:keys [rid body store]}]
      (if-let [nm-in (some-> (:name body) str str/trim not-empty)]
        (let [seg (scenario-seg (str (java.util.UUID/randomUUID)) nm-in)
              bn  (scenario-branch rid seg)]
          (scenarios/create-scenario-branch! store {:parent "main" :name bn})
          (negotiated req {:scenario seg :name nm-in}))
        (negotiated req 400 {:error "scenario name required"})))))

(defn my-application-scenario-drop
  [req]
  (with-app req {:seg :path}
    (fn [{:keys [seg sb store]}]
      (governance/drop-branch! store sb)
      (negotiated req {:ok true :scenario seg}))))

(defn- with-scenario
  [req f]
  (with-app req {:seg :path}
    (fn [{:keys [seg sb body store]}]
      (f store sb seg body))))

(defn my-application-scenario-actions
  [req]
  (with-scenario req
    (fn [store bn seg _]
      (let [store (ctx/at store bn)
            d  (scenarios/scenario-definition store (seat/branch-schema store))]
        (negotiated req (assoc d :scenario seg
                               :sealed? (actions/scenario-sealed? store)))))))

(defn my-application-scenario-seal
  [req]
  (with-scenario req
    (fn [store bn seg _]
      (scenarios/seal-scenario! (ctx/at store bn))
      (negotiated req {:ok true :scenario seg :sealed? true}))))

(defn my-application-scenario-rebuild
  [req]
  (with-scenario req
    (fn [store bn seg body]
      (negotiated req
                  (assoc
                   (scenarios/rebuild-scenario! store bn "main" {:force? (boolean (:force body))
                                                                :omit   (:omit body)})
                   :scenario seg)))))

(defn my-application-scenario-apply
  [req]
  (with-scenario req
    (fn [store bn seg body]
      (negotiated req
                  (assoc
                   (scenarios/apply-scenario! store bn "main" {:again? (boolean (:again body))})
                   :scenario seg)))))

(defn my-application-scenario-duplicate
  [req]
  (with-scenario req
    (fn [store bn _seg body]
      (if-let [nm-in (some-> (:name body) str str/trim not-empty)]
        (let [rid     (get-in req [:path-params :rid])
              new-seg (scenario-seg (str (java.util.UUID/randomUUID)) nm-in)
              new-bn  (scenario-branch rid new-seg)]
          (scenarios/duplicate-scenario! store bn "main" new-bn)
          (negotiated req {:scenario new-seg :name nm-in :from bn}))
        (negotiated req 400 {:error "scenario name required"})))))
