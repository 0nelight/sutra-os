(ns sutra.feedback.http
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.foundation.encoding :as enc]
            [sutra.foundation.ops :as op]
            [sutra.catalog.lookup :as lookup]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok]]
            [sutra.store.control :as ctl]
            [sutra.catalog.grants :as grants]
            [sutra.feedback.places :as places]
            [sutra.types.feedback :as b]))

(set! *warn-on-reflection* true)

(defn- admin? [u] (= u grants/admin-username))

(defn- wire [s]
  (reduce (fn [s k] (cond-> s (s k) (update k #(str (java.time.Instant/ofEpochMilli (inst-ms %))))))
          s [:begonnen :beendet :erledigt]))

(defn- denied [status msg] {:status status :body {:error msg}})

(defn- access
  [store u id write?]
  (let [s (when id (ctl/get store :begleitungen {:id id}))]
    (cond
      (nil? u) [nil (refusal/refused :refusal.feedback/niemand-angemeldet {})]
      (nil? s) [nil (refusal/refused :refusal.feedback/keine-solche-feedback-sitzung {:id id})]
      (= u (:benutzer s)) [s nil]
      (and (admin? u) (not write?)) [s nil]
      :else [nil (denied 403 "die Feedback-Sitzung eines anderen Benutzers")])))

(def start
  (ep/handler :feedback
              {:needs-user true
               :respond (fn [{:keys [store body] u :user}]
                          (let [s (b/sitzung (str (random-uuid)) u (:aufgabe body)
                                             (ctl/clock store))]
                            (ctl/insert! store :begleitungen s)
                            (ok (wire s))))}))

(defn- full-id
  [store id]
  (if (ctl/get store :begleitungen {:id id})
    id
    (let [matches (filter #(str/starts-with? (:id %) id) (ctl/list store :begleitungen {}))]
      (if (= 1 (count matches)) (:id (first matches)) id))))

(defn- with-title
  [schema {:keys [app pfad] :as k}]
  (let [rid  (some->> app keyword (lookup/rid-of schema :applications))
        steps (some-> pfad enc/edn-or-nil)
        node (when (and rid (sequential? steps))
               (get-in schema (into [:applications rid :layout] steps)))
        t    (when (map? node) (or (:title node) (:label node)))]
    (cond-> k (string? t) (assoc :titel t))))

(defn- resolve-references [schema e]
  (cond-> e
    (seq (:referenzen e))
    (update :referenzen
            (partial mapv #(cond-> % (:knoten %) (update :knoten (partial with-title schema)))))))

(defn- one-session [store schema u id]
  (let [id (full-id store id)
        [s refused] (access store u id false)]
    (or refused
        (let [es (mapv (comp (partial resolve-references schema) :ereignis)
                       (ctl/list store :begleitung-ereignisse {:sitzung id}))]
          (ok {:sitzung (wire s)
               :verlauf (b/verlauf es)
               :zusammenfassung (b/zusammenfassung s es)})))))

(defn with-histories
  [sessions rows]
  (let [per-session (group-by :sitzung rows)]
    (mapv (fn [s] (assoc s :verlauf (b/verlauf (map :ereignis (get per-session (:id s))))))
          sessions)))

(def read-sessions
  (ep/handler :feedback
              {:needs-user true
               :respond (fn [{:keys [store schema id verlauf] u :user :as a}]
                          (if id
                            (one-session store schema u id)
                            (let [ss (mapv wire (ctl/list store :begleitungen
                                                          (if (admin? u)
                                                            (select-keys a [:benutzer])
                                                            {:benutzer u})))]
                              (ok {:sitzungen
                                   (if verlauf
                                     (with-histories
                                       ss
                                       (map #(update % :ereignis
                                                     (partial resolve-references schema))
                                            (ctl/list store :begleitung-ereignisse {})))
                                     ss)}))))}))

(defn- event-row [session-id e]
  {:sitzung session-id :id (:id e) :art (:art e) :t (:t e) :ereignis e})

(def record-events
  (ep/handler :feedback-ereignisse
              {:needs-user true
               :respond (fn [{:keys [store body] u :user}]
                          (let [[s refused] (access store u (:sitzung body) true)]
                            (cond
                              refused refused
                              (:beendet s) (denied 409 "die Feedback-Sitzung ist beendet")
                              :else
                              (let [{:keys [angenommen abgelehnt]} (b/stapel (:ereignisse body))]
                                (doseq [e (map places/resolve-event angenommen)]
                                  (ctl/upsert! store :begleitung-ereignisse (event-row (:id s) e)))
                                (ok {:angenommen (count angenommen) :abgelehnt abgelehnt})))))}))

(def finish
  (ep/handler :feedback-ende
              {:needs-user true
               :respond (fn [{:keys [store body] u :user}]
                          (let [[s refused] (access store u (:sitzung body) true)
                                {f :wert fehler :fehler} (some-> (:fazit body) b/fazit)]
                            (cond
                              refused   refused
                              fehler (refusal/refused :refusal.feedback/das-fazit-ist-unvollstaendig {:fehler fehler})
                              :else  (let [s (cond-> (assoc s :beendet (ctl/clock store))
                                               f (assoc :fazit f))]
                                       (ctl/upsert! store :begleitungen s)
                                       (ok (wire s))))))}))

(def settle
  (ep/handler :feedback-erledigt
              {:respond (fn [{:keys [store body] u :user}]
                          (let [id (some->> (:sitzung body) (full-id store))
                                [s refused] (access store u id false)
                                sets (if (false? (:erledigt body))
                                       {:erledigt nil :erledigt-von nil}
                                       {:erledigt (ctl/clock store) :erledigt-von u})]
                            (cond
                              (not (admin? u)) (denied 403 "nur der Admin hakt Feedback ab")
                              refused refused
                              :else (do (ctl/update! store :begleitungen {:id (:id s)} sets)
                                        (ok (wire (merge s sets)))))))}))

(defn open-reports
  [store]
  (if-let [u (grants/username-or-admin store)]
    (frequencies
     (for [s (ctl/list store :begleitungen (if (admin? u) {} {:benutzer u}))
           :when (nil? (:erledigt s))
           {e :ereignis} (ctl/list store :begleitung-ereignisse {:sitzung (:id s)})
           :when (= :meldung (some-> (:art e) keyword))
           :let [place (:ort e)]
           ref (distinct (concat (for [o (:objekte place) :when (:typ o)]
                                   [:object-type (name (:typ o))])
                                 (when-let [a (:app place)] [[:application (name a)]])))]
       ref))
    {}))

(op/defop :issues/open-by-node [{:keys [store]} _]
  (open-reports store))

(def module
  {:name        :feedback
   :contributes {:http/routes {:feedback            {:get read-sessions :post start}
                               :feedback-ereignisse {:post record-events}
                               :feedback-ende       {:post finish}
                               :feedback-erledigt   {:post settle}}}})
