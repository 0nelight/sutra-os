(ns sutra.documents.use-case
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.store.access.scope :as ctx]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.html :refer [esc]]
            [sutra.types.rid :as rid]
            [sutra.authoring.api :as authoring]
            [sutra.lineage.api :as lineage]
            [sutra.documents.records :as docs]
            [sutra.parser.ent :as ent]
            [sutra.parser.project-export :as pe]
            [sutra.types.plan :as cp]
            [sutra.use-case.draft :as draft])
  (:import (java.util Base64)
           (java.util.concurrent TimeUnit)))

(set! *warn-on-reflection* true)

(defn- names-by-rid [schema section]
  (let [nf (rid/name-field-of section)]
    (into {} (for [[r s] (get schema section)] [r (->kw (or (get s nf) r))]))))

(defn- upstream [edges]
  (reduce (fn [m {:keys [from to]}] (update m to (fnil conj #{}) from)) {} edges))

(defn- walk-up [up start depth]
  (loop [seen #{} frontier #{start} d 0]
    (if (or (empty? frontier) (and depth (>= d depth)))
      seen
      (let [nxt (into #{} (comp (mapcat up) (remove seen)) frontier)]
        (recur (into seen nxt) nxt (inc d))))))

(defn- of-kind [kind ids]
  (let [p (str kind "/")]
    (into #{} (keep #(when (str/starts-with? % p) (keyword (subs % (count p))))) ids)))

(defn- attribute-names [spec]
  (into #{} (map #(->kw (name (or (:attribute-name %) (first %)))))
        (:attribute-list spec)))

(defn live-index
  [schema]
  (let [edges  (lineage/edges schema nil)
        up     (upstream edges)
        edited (into #{} (keep #(when (= :edits (:rel %)) (:to %))) edges)
        ots    (names-by-rid schema :object-types)]
    {:object-types
     (into {}
           (for [[r s] (:object-types schema)
                 :let [n    (ots r)
                       node (str "object-type/" (name n))
                       far  (walk-up up node nil)
                       near (walk-up up node 3)]]
             [n {:pk         (some-> (or (:primary-key s) (first (:match-keys s))) ->kw)
                 :attributes (attribute-names s)
                 :log?       (or (str/starts-with? (str (:display-name s)) "[LOG] ")
                                 (str/ends-with? (name n) "-log"))
                 :sourced?   (boolean (seq (of-kind "source" far)))
                 :transform? (boolean (seq (of-kind "transform" near)))
                 :edited?    (contains? edited node)
                 :reads      (disj (of-kind "object-type" near) n)}]))
     :link-types
     (let [lts (names-by-rid schema :link-types)]
       (into {} (for [[r l] (:link-types schema)]
                  [(lts r) {:from (ots (:from-object-type l) (->kw (:from-object-type l)))
                            :to   (ots (:to-object-type l) (->kw (:to-object-type l)))
                            :cardinality (->kw (:cardinality l))}])))
     :actions
     (let [as (names-by-rid schema :actions)]
       (into {} (for [[r a] (:actions schema)]
                  [(as r) {:on (some-> (:on a) ots) :webhooks? (boolean (seq (:webhooks a)))}])))
     :groups
     (let [gs (names-by-rid schema :object-type-groups)]
       (into {} (for [[r g] (:object-type-groups schema)]
                  [(gs r) (into #{} (map #(ots % (->kw %))) (:members g))])))
     :sources (set (vals (names-by-rid schema :sources)))}))

(defn draft-of
  [store auth id]
  (let [d (or (docs/document store auth (str id) :read)
              (when-not (= pe/trunk-branch (str (:branch store)))
                (docs/document (ctx/at store pe/trunk-branch) auth (str id) :read))
              (refusal/refuse! :refusal.documents/no-document {:id id}))
        m (draft/read-draft (:body d))]
    (when-let [e (::draft/error m)]
      (refusal/refuse! :refusal.documents/draft-invalid {:id id} {:draft (:name d) :why e}))
    {:document (dissoc d :body) :draft m}))

(def ^:private seconds 20)

(defn svg
  [^String dot]
  (let [cmd ^"[Ljava.lang.String;" (into-array String ["dot" "-Tsvg"])
        p   (.start (ProcessBuilder. cmd))
        out (future (slurp (.getInputStream p) :encoding "UTF-8"))
        err (future (slurp (.getErrorStream p) :encoding "UTF-8"))]
    (with-open [w (io/writer (.getOutputStream p) :encoding "UTF-8")]
      (.write w dot))
    (if-not (.waitFor p seconds TimeUnit/SECONDS)
      (do (.destroyForcibly p)
          (refusal/refuse! :refusal.documents/graphviz-timeout {:seconds seconds}))
      (if (zero? (.exitValue p))
        @out
        (anomaly/raise! :fault (str "graphviz: " (str/trim @err)) {})))))

(defn- figure [svg-text caption]
  (str "<figure class=\"np-diagram\"><img alt=\"" (esc caption) "\" src=\""
       "data:image/svg+xml;base64,"
       (.encodeToString (Base64/getEncoder) (.getBytes ^String svg-text "UTF-8")) "\"/>"
       "<figcaption><span class=\"np-diagram__caption\">" (esc caption)
       "</span></figcaption></figure>"))

(def ^:private op-label
  {:create "anlegen" :change "ändern" :retire "zurückbauen" :keep "vorhanden" :check "prüfen"})

(def ^:private kind-label
  {:object-type "Objekttyp" :link-type "Link-Typ" :action-type "Action" :source "Quelle"
   :writeback "Rückschreiben" :object-type-group "Gruppe"})

(defn plan-html
  [{:keys [steps hints]}]
  (str "<table class=\"np-objtable np-plan\"><thead><tr><th>Schritt</th><th>Art</th>"
       "<th>Name</th><th>selbst</th><th>Begründung</th></tr></thead><tbody>"
       (str/join
        (for [{:keys [op kind name auto? why warn]} steps]
          (str "<tr class=\"np-plan--" (clojure.core/name op) "\"><td>" (esc (op-label op))
               "</td><td>" (esc (kind-label kind)) "</td><td><code>" (esc (clojure.core/name name))
               "</code></td><td>" (if auto? "✓" "") "</td><td>" (esc why)
               (str/join (for [w warn] (str "<br/><span class=\"np-missing\">⚠ " (esc w)
                                            "</span>")))
               "</td></tr>")))
       "</tbody></table>"
       (when (seq hints)
         (str "<ul class=\"np-plan__hints\">"
              (str/join (for [h hints] (str "<li>" (esc h) "</li>")))
              "</ul>"))))

(defn widget
  [store auth schema {:keys [view chart] :as attrs}]
  (let [{:keys [draft document]} (draft-of store auth (:draft attrs))
        live (live-index schema)]
    (case (or (not-empty view) "object-model")
      "object-model"
      (if-let [om (:object-model draft)]
        (figure (svg (draft/object-model-dot om live))
                (str "Objektmodell aus dem Entwurf „" (:name document) "“"))
        "<span class=\"np-missing\">Der Entwurf hat kein :object-model</span>")
      "statechart"
      (if-let [sc (draft/statechart draft chart)]
        (figure (svg (draft/statechart-dot sc live))
                (str (or (:title sc) "Lebenszyklus") " · Entwurf „" (:name document) "“"))
        (str "<span class=\"np-missing\">kein Lebenszyklus " (esc chart) " im Entwurf</span>"))
      "plan"
      (plan-html (draft/plan draft live))
      (str "<span class=\"np-missing\">unbekannte Ansicht " (esc view) "</span>"))))

(defn plan
  [{:keys [store auth schema branch]} id {:keys [preview?]}]
  (let [{:keys [draft document]} (draft-of store auth id)
        p      (draft/plan draft (live-index schema))
        inst   (draft/install-schema p)
        bad    (when (seq inst) (ent/explain-schema inst))
        p      (cond-> p bad (update :hints conj
                                     (str "Was der Assistent anlegen würde, besteht die Grammatik "
                                          "nicht: " (pr-str bad))))]
    (cond-> (assoc p :document document :summary (draft/summary p)
                   :project (:project draft)
                   :install inst :retire (draft/retirements p)
                   :findings (if bad
                               (cp/problems :use-case/grammar
                                            [(str "Was der Assistent anlegen würde, besteht die "
                                                  "Grammatik nicht: " (pr-str bad))])
                               []))
      (and preview? (seq inst) (not bad))
      (assoc :preview (authoring/stage! {:store store :schema schema :branch branch
                                     :project (some-> (:project draft) name)}
                                    {:schema inst :preview? true} nil)))))

(defn apply!
  [{:keys [store schema branch] :as ctx} id]
  (when (or (str/blank? (str branch)) (= pe/trunk-branch (str branch)))
    (refusal/refuse! :refusal.documents/use-case-on-trunk {:branch branch}))
  (let [p    (plan ctx id {})
        inst (:install p)]
    (assoc (select-keys p [:summary :retire :steps :hints])
           :branch branch
           :staged (cp/apply! (cp/plan (when (seq inst) [inst]) (:findings p))
                              (fn [[inst]]
                                (when inst
                                  (authoring/stage! {:store store :schema schema :branch branch
                                                     :project (some-> (:project p) name)}
                                                    {:schema inst} nil)))
                              {:code :refusal.documents/use-case-grammar}))))
