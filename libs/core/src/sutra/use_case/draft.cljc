(ns sutra.use-case.draft
  (:require [sutra.kit.coll :as coll]
            #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])
            [clojure.string :as str]))

(defn read-draft
  [text]
  (let [d (try (edn/read-string (str text))
               (catch #?(:clj Exception :cljs :default) e
                 {::error (str "kein EDN: " (ex-message e))}))]
    (cond (::error d) d
          (not (map? d)) {::error "der Entwurf ist keine Map"}
          (not (or (:object-model d) (seq (:statecharts d))))
          {::error "der Entwurf hat weder :object-model noch :statecharts"}
          :else d)))

(defn- kw [x] (cond (keyword? x) x (string? x) (keyword x) (symbol? x) (keyword (name x))))

(defn- one-line [s]
  (some-> s str (str/replace #"-\n" "") (str/replace #"\s*\n\s*" " ") str/trim))

(defn- plain-name? [s] (boolean (re-matches #"[a-z][a-z0-9-]*" (str s))))

(defn types [d] (get-in d [:object-model :types]))
(defn links [d] (get-in d [:object-model :links]))

(defn ontology-name
  [t]
  (kw (or (:object-type t) (:id t))))

(defn type-status
  [live t]
  (let [there? (contains? (:object-types live) (ontology-name t))]
    (if (= :retired (:kind t))
      (if there? :retire :gone)
      (if there? :exists :new))))

(def cardinality
  {"N:1" :many-to-one "N:0..1" :many-to-one "0..N:1" :many-to-one "N:0..N" :many-to-many
   "1:N" :one-to-many "0..1:N" :one-to-many "1:1" :one-to-one "1:0..1" :one-to-one
   "0..1:1" :one-to-one "N:N" :many-to-many "N:M" :many-to-many})

(defn- live-link
  [live a b]
  (some (fn [[n {:keys [from to] :as l}]]
          (when (or (and (= a from) (= b to)) (and (= a to) (= b from))) (assoc l :name n)))
        (:link-types live)))

(defn- sketch-link?
  [l]
  (and (= :dashed (kw (:style l))) (nil? (:card l))))

(def classes
  (array-map
   :core          {:fill "#dbe8f7" :line "#4a7fc1" :legend "Kern · Quellsystem"
                   :upstream? true
                   :why (fn [t] (str "Kern: braucht Sync und Transform"
                                     (when-let [s (:source t)] (str " aus Quelle " (name s)))
                                     " (Guide §3), erst dann anlegen"))
                   :rule (fn [{:keys [sourced?]} _]
                           (when-not sourced? ["Kern ohne Quelle stromauf"]))}
   :core-internal {:fill "#dff0d8" :line "#5f9c45" :legend "Kern · interner Betrieb"
                   :upstream? true}
   :derived       {:fill "#ece1f3" :line "#8a63ad" :legend "Abgeleitet · Anreicherung"
                   :upstream? true
                   :why (fn [_] (str "Abgeleitet: braucht einen Transform (Guide §3), "
                                     "erst dann anlegen"))
                   :rule (fn [{:keys [transform? edited?]} _]
                           (concat (when-not transform? ["Abgeleitet ohne Transform"])
                                   (when edited? ["Abgeleitet, aber eine Action ändert ihn"])))}
   :use-case      {:fill "#fdebc8" :line "#d9942a" :legend "Use Case · operativ bearbeitet"
                   :auto? true
                   :why (fn [_] "Use-Case-Typ: wird von Actions geschrieben")
                   :rule (fn [{:keys [edited?]} written?]
                           (when-not (or edited? written?)
                             ["Use Case ohne schreibende Action"]))}
   :retired       {:fill "#eeeeee" :line "#9e9e9e" :legend "entfällt" :style "dashed"
                   :out? true}
   :report        {:fill "#ffffff" :line "#9e9e9e" :legend "→ App (Bericht, kein Typ)"
                   :style "dashed" :marker? true :out? true}
   :property-of   {:fill "#ffffff" :line "#9e9e9e" :legend "→ Eigenschaft (kein Typ)"
                   :style "dashed" :marker? true :out? true}))

(defn class-of
  [t]
  (or (:kind t) :core))

(defn- in-class? [facet t] (boolean (get-in classes [(class-of t) facet])))

(defn- default-why [t] ((get-in classes [:core :why]) t))

(def tones
  {nil    {:fill "#e8f0fb" :line "#4a7fc1"}
   :ok    {:fill "#e3f1dc" :line "#5f9c45"}
   :warn  {:fill "#fdf0d5" :line "#d9942a"}
   :muted {:fill "#f2f2f2" :line "#9e9e9e"}
   :neg   {:fill "#fbe3e1" :line "#c0504d"}})

(defn- q [s]
  (str "\"" (-> (str s) (str/replace "\\" "\\\\") (str/replace "\"" "\\\"")
                (str/replace "\n" "\\n"))
       "\""))

(defn- html [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;") (str/replace "\n" "<br/>")))

(defn- title-label [title note]
  (str "<<b>" (html title) "</b>"
       (when note (str "<br/><font point-size=\"11\" color=\"#555555\">" (html note) "</font>"))
       ">"))

(defn- attrs [m]
  (str "[" (str/join ", " (for [[k v] m :when (some? v)] (str (name k) "=" v))) "]"))

(defn- id [k] (q (if (keyword? k) (name k) (str k))))

(def ^:private status-note
  {:new "noch nicht angelegt" :retire "wird zurückgebaut" :gone nil :exists nil})

(defn- circle-width
  [{:keys [label pk]} note]
  (let [title (apply max 0 (map count (str/split-lines (str label))))
        small (apply max 0 (map count (concat [(str "PK " pk)]
                                              (str/split-lines (str note)))))
        w     (max (* title 0.1) (* small 0.068))]
    (/ (Math/round (* 100 (max 1.7 (+ w 0.45)))) 100.0)))

(defn- type-node [live {:keys [label pk note kind domain] :as t}]
  (let [st   (when (and live (not (in-class? :marker? t))) (type-status live t))
        note (str/join "\n" (remove nil? [(some->> domain (str "Kern · ")) note
                                           (when (= :property-of kind)
                                             (some->> (:of t) name (str "an ")))]))
        note (not-empty note)
        {:keys [fill line style]} (classes (class-of t))
        dash (or style (when (= :new st) "dashed"))
        text (str "<<b>" (html (or label (name (:id t)))) "</b>"
                  (when pk (str "<br/><font point-size=\"9\">PK " (html pk) "</font>"))
                  (when note (str "<br/><font point-size=\"9\" color=\"#555555\">"
                                  (html note) "</font>"))
                  (when-let [s (status-note st)]
                    (str "<br/><font point-size=\"8\" color=\"#ac2f33\">" (html s) "</font>"))
                  ">")]
    (str "  " (id (:id t)) " "
         (attrs {:label text :shape "circle" :style (q (str "filled" (when dash (str "," dash))))
                 :fillcolor (q fill) :color (q line) :penwidth 1.6
                 :width (circle-width t note) :fixedsize "true"
                 :fontname (q "Helvetica") :fontsize 11})
         ";")))

(defn- link-edge [live by-id {:keys [from to card label style] :as l}]
  (let [missing? (and live (not (sketch-link? l))
                      (nil? (live-link live (ontology-name (by-id from))
                                       (ontology-name (by-id to)))))]
    (str "  " (id from) " -> " (id to) " "
         (attrs {:label (q (str/join " " (remove nil? [label card (when missing? "· neu")])))
                 :fontsize 9 :fontname (q "Helvetica") :color (q "#777777")
                 :fontcolor (q (if missing? "#ac2f33" "#444444")) :arrowhead "none"
                 :style (some-> (or (some-> style name) (when missing? "dashed")) q)})
         ";")))

(defn- legend [used live?]
  (str "  legende [shape=plaintext, fontname=\"Helvetica\", label=<"
       "<table border=\"1\" cellborder=\"0\" cellspacing=\"3\" color=\"#999999\">"
       (str/join (for [[k {:keys [fill line legend]}] classes
                       :when (used k)]
                   (str "<tr><td bgcolor=\"" fill "\" width=\"14\" height=\"14\" "
                        "style=\"rounded\" color=\"" line "\" border=\"1\"></td>"
                        "<td align=\"left\"><font point-size=\"10\">" (html legend)
                        "</font></td></tr>")))
       (when live?
         (str "<tr><td></td><td align=\"left\"><font point-size=\"9\" color=\"#ac2f33\">"
              "gestrichelt = noch nicht in der Ontologie</font></td></tr>"))
       "</table>>];"))

(defn object-model-dot
  ([om] (object-model-dot om nil))
  ([{:keys [title] ts :types ls :links} live]
   (let [by-id (coll/index-by :id ts)]
     (str/join
      "\n"
      (concat
       ["digraph objektmodell {"
        (str "  graph " (attrs {:layout "neato" :overlap "false" :splines "true" :sep (q "+18")
                                :label (title-label (or title "Skizze des Objektmodells") nil)
                                :labelloc "t" :fontsize 16 :fontname (q "Helvetica")
                                :pad 0.4 :bgcolor (q "white")}) ";")]
       (map #(type-node live %) ts)
       (for [l ls :when (and (by-id (:from l)) (by-id (:to l)))] (link-edge live by-id l))
       [(legend (set (map class-of ts))
                (and live (some #(#{:new} (type-status live %)) ts)))
        "}"])))))

(defn- state-node [{:keys [label note tone kind] :as s}]
  (case kind
    :initial (str "  " (id (:id s)) " [shape=circle, style=filled, fillcolor=\"#333333\", "
                  "label=\"\", width=0.18, fixedsize=true];")
    :final   (str "  " (id (:id s)) " [shape=doublecircle, style=filled, fillcolor=\"#333333\", "
                  "label=\"\", width=0.14, fixedsize=true];")
    (let [{:keys [fill line]} (tones tone)
          text (str "<<b>" (html (or label (name (:id s)))) "</b>"
                    (when note (str "<br/><font point-size=\"9\" color=\"#555555\">"
                                    (str/join "<br/>" (map html (str/split-lines note)))
                                    "</font>"))
                    ">")]
      (str "  " (id (:id s)) " "
           (attrs {:label text :shape "box" :style (q "rounded,filled")
                   :fillcolor (q fill) :color (q line) :penwidth 1.4
                   :fontname (q "Helvetica") :fontsize 11 :margin (q "0.18,0.08")})
           ";"))))

(declare state-block)

(defn- cluster [{:keys [label note parallel? children tone] :as s} depth]
  (let [{:keys [line]} (tones tone)]
    (str/join
     "\n"
     (concat
      [(str "  subgraph " (q (str "cluster_" (name (:id s)))) " {")
       (str "    graph " (attrs {:label (q (str label (when parallel? "  ‖ parallel")
                                          (when note (str "\n" note))))
                           :style (q (if parallel? "rounded,dashed" "rounded"))
                           :color (q (or line "#4a7fc1")) :fontname (q "Helvetica-Bold")
                           :fontsize 12 :labeljust "l" :margin 14
                           :bgcolor (q (if (even? depth) "#fbfcfe" "#ffffff"))}) ";")]
      (map #(state-block % (inc depth)) children)
      ["  }"]))))

(defn- state-block [s depth]
  (if (seq (:children s)) (cluster s depth) (state-node s)))

(defn- find-state [states k]
  (some #(if (= k (:id %)) % (find-state (:children %) k)) states))

(defn- leaf-of
  [states k]
  (let [first-leaf (fn fl [s] (if (seq (:children s)) (fl (first (:children s))) s))]
    (when-let [s (find-state states k)]
      [(:id (first-leaf s)) (when (seq (:children s)) (str "cluster_" (name k)))])))

(defn- transition-label [{:keys [label action role guard]}]
  (str (or label (some->> action name (str "Action: ")))
       (when role (str "\n(" role ")"))
       (when guard (str "\n[" guard "]"))))

(defn- transition-edge [live states {:keys [from to style action] :as tr}]
  (let [[f fc] (leaf-of states from)
        [t tc] (leaf-of states to)
        missing? (and live action (not (contains? (:actions live) (kw action))))]
    (if-not (and f t)
      (str "  // unbekannter Zustand in " from " -> " to)
      (str "  " (id f) " -> " (id t) " "
           (attrs {:label (q (str (transition-label tr) (when missing? "\n· neu")))
                   :ltail (some-> fc q) :lhead (some-> tc q)
                   :fontname (q "Helvetica") :fontsize 9
                   :fontcolor (q (if missing? "#ac2f33" "#333333"))
                   :color (q "#555555") :arrowsize 0.7 :style (some-> style name q)})
           ";"))))

(defn statechart-dot
  ([sc] (statechart-dot sc nil))
  ([{:keys [title note states transitions rankdir type property]} live]
   (str/join
    "\n"
    (concat
     ["digraph statechart {"
      (str "  graph " (attrs {:compound "true" :rankdir (or rankdir "TB") :nodesep 0.5
                              :ranksep 0.55
                              :label (title-label (or title (str "Lebenszyklus "
                                                                 (some-> type name)))
                                                  (or note (when property
                                                             (str "Zustand in der Eigenschaft "
                                                                  (name property)))))
                              :labelloc "t" :fontsize 16 :fontname (q "Helvetica")
                              :pad 0.4 :bgcolor (q "white")}) ";")]
     (map #(state-block % 0) states)
     (map #(transition-edge live states %) transitions)
     ["}"]))))

(defn statechart [d chart-id]
  (let [scs (:statecharts d)]
    (if (str/blank? (str chart-id))
      (first scs)
      (some #(when (= (kw chart-id) (kw (:id %))) %) scs))))

(defn- step [op kind nm m] (merge {:op op :kind kind :name nm :auto? false} m))

(defn- pk-of [t]
  (let [pk (:pk t)]
    (if (plain-name? pk) (keyword pk) (keyword (str (name (ontology-name t)) "-id")))))

(defn- state-value [states k]
  (let [s (find-state states k)]
    (when (and s (not (#{:initial :final} (:kind s))))
      (name (:id s)))))

(defn- initial? [states k] (= :initial (:kind (find-state states k))))

(defn- drawn-name
  [label]
  (when-not (str/starts-with? (str label) "Action:") (one-line label)))

(defn- lifecycle-order
  [d]
  (distinct (for [sc (:statecharts d) :when (:type sc)
                  {:keys [action]} (:transitions sc) :when action]
              (kw action))))

(defn- lifecycle-actions
  [d]
  (reduce
   (fn [acc {:keys [type property states transitions]}]
     (let [t (kw type) p (kw (or property :status))
           creates? (= :use-case (some #(when (= t (ontology-name %)) (:kind %)) (types d)))]
       (reduce (fn [acc {:keys [from to action label role]}]
                 (if-let [a (kw action)]
                   (let [tv (state-value states to)
                         fv (state-value states from)]
                     (update acc a
                             (fn [m]
                               (cond-> (merge {:type t :property p :to #{} :from #{}
                                               :label (drawn-name label) :role role}
                                              m)
                                 tv                      (update :to conj tv)
                                 fv                      (update :from conj fv)
                                 (and creates? (initial? states from))
                                 (assoc :create? true)))))
                   acc))
               acc transitions)))
   {} (filter :type (:statecharts d))))

(defn- humanize [k] (-> (name k) (str/replace "-" " ") str/capitalize))

(defn action-skeleton
  [a {:keys [type property to from create? label]} pk]
  (let [tos (vec (sort to))
        one (when (= 1 (count tos)) (first tos))
        val (if one one [:get :ergebnis])
        res (when-not one
              {:ergebnis {:type :string :name "Ergebnis" :required true
                         :constraints {:one-of tos}}})]
    (if create?
      {:display-name (or label (humanize a))
       :params (merge {pk {:type :string :exposure :hidden :prefill :uuid}} res)
       :rules [{:op :create :object-type type :id [:get pk]
                :values (cond-> {pk [:get pk]} (seq tos) (assoc property val))}]}
      (cond-> {:display-name (or label (humanize a))
               :on type
               :params (merge {type {:type type :name (humanize type) :required true}} res)
               :rules [{:op :update :target [:get type :id] :object-type type
                        :values {property val}}]}
        (seq from)
        (assoc :submission-criteria
               [{:test {:s [:get type property]}
                 :pred (into [:in :s] (sort from))
                 :message (str "nur aus " (str/join ", " (sort from)))}])))))

(defn- object-spec [t props]
  (let [pk    (pk-of t)
        given (vec (:attributes t))
        have  (set (map first given))
        attrs (cond-> given
                (not (have pk)) (->> (into [[pk :string]]))
                true            (into (for [p (sort props) :when (not (have p))
                                            :when (not= p pk)]
                                        [p :string])))]
    {:name   (or (one-line (:label t)) (humanize (ontology-name t)))
     :plural (or (:plural t) (one-line (:label t)) (humanize (ontology-name t)))
     :title  (kw (or (:title t) pk))
     :primary-key pk
     :attributes attrs}))

(defn- type-steps [d live state-props]
  (for [t (types d)
        :let [n  (ontology-name t)
              st (type-status live t)
              props (get state-props n)]
        :when (and (not= :gone st) (not (get-in live [:object-types n :log?])))]
    (case (if (in-class? :marker? t) (:kind t) st)
      :report
      (step :check :object-type n
            {:why (str "kein Objekttyp: ein Bericht gehört in eine App mit Aggregation zur "
                       "Lesezeit (Guide §3)")
             :warn (if (= :exists st) [(str n " ist installiert — Rückbau prüfen")] [])})

      :property-of
      (step :check :object-type n
            {:why (str "kein Objekttyp: Ergänzung (:role :supp) oder :derived an "
                       (some-> (:of t) name))
             :warn (if (= :exists st) [(str n " ist installiert — Rückbau prüfen")] [])})

      :retire
      (step :retire :object-type n {:auto? true :why "im Entwurf als entfallend gezeichnet"})

      :exists
      (let [have    (get-in live [:object-types n :attributes])
            missing (vec (sort (remove have (concat (map first (:attributes t)) props))))]
        (if (seq missing)
          (step :change :object-type n
                {:why  (str "fehlende Eigenschaften: " (str/join ", " (map name missing)))
                 :add  missing
                 :hint (str "`ontology object-type edit " (name n) "` (eine Teil-Installation "
                            "würde die übrigen Felder leeren)")})
          (step :keep :object-type n {:why "gibt es schon"})))

      :new
      (let [kind  (class-of t)
            spec  (object-spec t props)
            pk    (:primary-key spec)
            twin  (some (fn [[o {opk :pk}]] (when (= pk opk) o)) (:object-types live))
            warns (cond-> []
                    twin (conj (str "Schattentyp? " (name twin) " hat denselben Schlüssel "
                                    (name pk) " (System Silo, Guide §3)"))
                    (and (:pk t) (not (plain-name? (:pk t))))
                    (conj (str "Schlüssel " (pr-str (:pk t)) " ist kein Eigenschaftsname, "
                               "vorgeschlagen " (name pk))))]
        (step :create :object-type n
              {:auto? (in-class? :auto? t)
               :spec  spec
               :class kind
               :warn  warns
               :why   ((get-in classes [kind :why] default-why) t)})))))

(defn- link-steps [d live type-plan]
  (let [by-id (coll/index-by :id (types d))]
    (for [l (links d)
          :let [a (by-id (:from l)) b (by-id (:to l))]
          :when (and a b (not (sketch-link? l))
                     (not-any? #(in-class? :out? %) [a b]))
          :let [fa (ontology-name a) fb (ontology-name b)
                there (live-link live fa fb)
                nm (kw (or (:link-type l) (str (name fa) "-" (name fb))))
                card (get cardinality (str (:card l)))
                ready? (fn [t] (#{:exists :auto} (type-plan t)))]]
      (cond
        there (step :keep :link-type (:name there) {:why (str (name fa) " ↔ " (name fb))})
        (nil? card)
        (step :create :link-type nm
              {:warn [(str "Kardinalität " (pr-str (:card l)) " unbekannt — N:1, 1:N, 1:1 "
                           "oder N:N angeben (Guide §4: gemessen, nicht geraten)")]
               :why (str (name fa) " → " (name fb))})
        :else
        (step :create :link-type nm
              {:auto? (boolean (and (ready? fa) (ready? fb)))
               :spec  [nm fa fb card]
               :why   (str (name fa) " → " (name fb) " " (:card l)
                           (when-not (and (ready? fa) (ready? fb))
                             " — wartet, bis beide Typen angelegt sind"))})))))

(defn- action-steps
  [d live type-plan lc pks pending]
  (let [given (:actions d)]
    (for [a (distinct (concat (lifecycle-order d) (keys given)))
          :let [info (get lc a)
                t    (or (:type info) (kw (:on (get given a))))
                spec (or (get given a)
                         (action-skeleton a info (get pks t (keyword (str (name t) "-id")))))
                lacks (when info (get-in pending [t (:property info)]))
                ready? (and (#{:exists :auto} (type-plan t)) (not lacks))]]
      (if (contains? (:actions live) a)
        (step :keep :action-type a {:why (str "gibt es schon"
                                              (when (and (get given a) info)
                                                " — Abweichungen bitte von Hand prüfen"))})
        (step :create :action-type a
              {:auto? (boolean ready?)
               :spec  spec
               :role  (:role info)
               :why   (str (if (get given a) "aus dem Entwurf" "aus dem Lebenszyklus erzeugt")
                           (when info (str ": " (name (:property info)) " → "
                                           (str/join " · " (sort (:to info)))))
                           (cond
                             (not (#{:exists :auto} (type-plan t)))
                             (str " — wartet auf Typ " (some-> t name))
                             lacks
                             (str " — wartet auf Eigenschaft " (name t) "."
                                  (name (:property info)))))})))))

(defn- source-steps [d live]
  (for [[s {:keys [type note]}] (:sources d)]
    (if (contains? (:sources live) (kw s))
      (step :keep :source (kw s) {:why "Quelle ist angebunden"})
      (step :create :source (kw s)
            {:why (str "Quelle fehlt" (when type (str " (" (name type) ")"))
                       (when note (str ": " note))
                       " — Quellen gelten instanzweit und tragen Geheimnisse: `source add`, "
                       "dann `source secret " (name s) "`")}))))

(defn- writeback-steps [d live planned]
  (for [{:keys [action system via note]} (:writebacks d)
        :let [a (kw action) la (get-in live [:actions a]) via (kw via)]]
    (step :check :writeback a
          {:system system :via via
           :why (str (or system "?") " über " (some-> via name)
                     (when note (str " — " note)))
           :warn (cond-> []
                   (and (nil? la) (not (planned a)))
                   (conj (str "keine Action " (name a) " — Zeile ohne Entsprechung (Guide §8)"))
                   (and la (= :webhook via) (not (:webhooks? la)))
                   (conj (str (name a) " hat keinen Webhook"))
                   (and (nil? la) (= :webhook via))
                   (conj "Webhook nach dem Anlegen eintragen; Quelle braucht :writeback"))})))

(defn- lifecycle-hints [d live]
  (concat
   (for [{:keys [id type transitions]} (:statecharts d)
         tr transitions
         :when (and type (nil? (:action tr)) (not (initial? (:states (statechart d id))
                                                             (:from tr))))]
     (str "Lebenszyklus " (name id) ": Übergang " (some-> (:from tr) name) " → "
          (some-> (:to tr) name) " hat keine :action (Guide §6)"))
   (for [{:keys [id type]} (:statecharts d)
         :when (and type (not (some #(= (kw type) (ontology-name %)) (types d)))
                    (not (contains? (:object-types live) (kw type))))]
     (str "Lebenszyklus " (name id) ": Typ " (name type) " fehlt im Objektmodell"))
   (for [l (links d)
         :when (not (and (some #(= (:from l) (:id %)) (types d))
                         (some #(= (:to l) (:id %)) (types d))))]
     (str "Kante " (some-> (:from l) name) " → " (some-> (:to l) name)
          " verweist auf einen Typ, der nicht gezeichnet ist"))
   (for [t (types d) :when (nil? (:kind t))]
     (str "Typ " (name (:id t)) " hat keine Klasse (Kern, Abgeleitet, Use Case — Guide §3)"))))

(defn- class-rules
  [d live planned-actions lc]
  (let [kind-of (into {} (for [t (types d)] [(ontology-name t) (:kind t)]))
        writes  (into #{} (map :type) (vals (select-keys lc planned-actions)))]
    (for [t (types d)
          :let [n (ontology-name t) k (:kind t)
                {:keys [reads] :as lt} (get-in live [:object-types n])
                use-case-reads (filter #(= :use-case (kind-of %)) reads)]
          :when (and lt (not (:log? lt)))
          w (concat
             (when-let [rule (get-in classes [k :rule])] (rule lt (writes n)))
             (when (and (get-in classes [k :upstream?]) (seq use-case-reads))
               [(str "liest den Use-Case-Typ " (str/join ", " (map name use-case-reads))
                     " (Rückkopplung nur ausdrücklich erklärt)")]))]
      (str "Klasse " (name n) ": " w))))

(defn- group-step [d live type-plan]
  (when-let [g (kw (:group d))]
    (let [have    (get-in live [:groups g])
          wanted  (for [t (types d)
                        :let [n (ontology-name t)]
                        :when (and (not (in-class? :out? t))
                                   (#{:exists :auto} (type-plan n)))]
                    n)
          members (vec (sort (into (set have) wanted)))
          added   (remove (set have) members)]
      (cond
        (and have (empty? added))
        (step :keep :object-type-group g {:why (str (count have) " Mitglieder")})
        :else
        (step (if have :change :create) :object-type-group g
              {:auto? true
               :spec  {:members members
                       :description (str "Use Case: "
                                         (or (:use-case d) (get-in d [:object-model :title])
                                             (name g)))}
               :why   (str "Auswahl für das Objektmodell-Widget (data-group); neu: "
                           (str/join ", " (map name added)))})))))

(defn plan
  [d live]
  (let [lc          (lifecycle-actions d)
        state-props (reduce (fn [m {:keys [type property]}]
                              (cond-> m type (update (kw type) (fnil conj #{})
                                                     (kw (or property :status)))))
                            {} (:statecharts d))
        ts          (vec (type-steps d live state-props))
        type-plan   (into (into {} (for [t (keys (:object-types live))] [t :exists]))
                          (for [s ts]
                            [(:name s) (case (:op s)
                                         :create (if (:auto? s) :auto :proposed)
                                         :retire :retire
                                         :exists)]))
        pks         (into (into {} (for [[t {:keys [pk]}] (:object-types live) :when pk]
                                     [t pk]))
                          (for [t (types d)] [(ontology-name t) (pk-of t)]))
        pending     (into {} (for [s ts :when (= :change (:op s))]
                               [(:name s) (set (:add s))]))
        as          (vec (action-steps d live type-plan lc pks pending))
        planned     (set (map :name as))
        group       (group-step d live type-plan)]
    {:steps (vec (concat (source-steps d live)
                         ts
                         (link-steps d live type-plan)
                         as
                         (when group [group])
                         (writeback-steps d live planned)))
     :hints (vec (concat (lifecycle-hints d live)
                         (class-rules d live (set (map :name (filter :auto? as))) lc)
                         (when-not (:group d)
                           [(str ":group fehlt — mit einer Objekttyp-Gruppe zeichnet das "
                                 "Objektmodell-Widget die Typen später aus der Ontologie")])))}))

(def step-kinds
  {:link-type   {:retire-rank 0 :stage (fn [s _ spec] (update s :links (fnil conj []) spec))}
   :action-type {:retire-rank 1 :stage (fn [s nm spec] (assoc-in s [:actions nm] spec))}
   :object-type {:retire-rank 2 :stage (fn [s nm spec] (assoc-in s [:objects nm] spec))}})

(defn install-schema
  [{:keys [steps]}]
  (reduce (fn [s {:keys [op kind name spec auto?]}]
            (cond
              (and auto? (= :object-type-group kind) (#{:create :change} op))
              (assoc-in s [:object-type-groups name] spec)

              (not (and auto? (= :create op)))
              s

              :else
              (if-let [stage (get-in step-kinds [kind :stage])]
                (stage s name spec)
                s)))
          {} steps))

(defn retirements
  [{:keys [steps]}]
  (vec (for [k (sort-by (comp :retire-rank step-kinds) (keys step-kinds))
             {:keys [op kind name auto?]} steps
             :when (and auto? (= :retire op) (= k kind))]
         [kind name])))

(defn summary
  [{:keys [steps]}]
  (assoc (frequencies (map :op steps))
         :auto (count (filter :auto? steps))))
