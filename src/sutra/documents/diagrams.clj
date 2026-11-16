(ns sutra.documents.diagrams
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.documents.ports :as ports]
            [sutra.canvas.api :as canvas]
            [sutra.lineage.api :as lineage]
            [sutra.types.rid :as rid]
            [sutra.viz.graph-layout :as gl]
            [sutra.viz.graph-svg :as gsvg]
            [sutra.viz.layouts :as layouts]
            [sutra.viz.palette :as palette]
            [sutra.kit.html :refer [esc]])
  (:import (java.net URLEncoder)
           (java.util Base64)))

(set! *warn-on-reflection* true)

(defn- figure
  [svg {:keys [caption legend href]}]
  (str "<figure class=\"np-diagram\"><img alt=\"" (esc caption) "\" src=\""
       "data:image/svg+xml;base64,"
       (.encodeToString (Base64/getEncoder) (.getBytes ^String svg "UTF-8")) "\"/>"
       "<figcaption>"
       (str/join (for [[cls label] legend]
                   (str "<span class=\"np-legend " cls "\">" (esc label) "</span>")))
       "<span class=\"np-diagram__caption\">" (esc caption) "</span>"
       (when href (str " <a class=\"np-resource\" href=\"" (esc href) "\">öffnen</a>"))
       "</figcaption></figure>"))

(defn- names [s]
  (into [] (comp (map str/trim) (remove str/blank?)) (str/split (str s) #",")))

(defn- draw
  ([store nodes edges] (draw store nodes edges nil))
  ([store nodes edges prefer]
   (let [table (ports/viz-layouts store)]
     (gsvg/graph->svg {:nodes (if (contains? table prefer)
                                (layouts/place table nodes edges {:layout  prefer
                                                                  :richtung :links-rechts})
                                (gl/place nodes edges {}))
                       :edges edges}))))

(defn- graph-entry [schema handle]
  (let [h      (str/trim (str handle))
        graphs (:graphs schema)]
    (or (when-let [s (get graphs h)] [h s])
        (let [kw (rid/slug-kw h)]
          (first (for [[g s] graphs
                       :when (or (= kw (:graph-name s)) (= kw (some-> (:title s) rid/slug-kw)))]
                   [g s]))))))

(defn graph
  [store schema {:keys [graph]}]
  (let [[grid spec] (or (graph-entry schema graph)
                        (refusal/refuse! :refusal.documents/no-saved-graph {} {:graph graph}))]
    (when-not (= :lineage (->kw (:mode spec)))
      (refusal/refuse! :refusal.documents/lineage-graph-only {:graph grid}))
    (figure (lineage/graph-svg {:store     store
                                :schema    schema
                                :scope     (:visible-types spec)
                                :hidden    (get-in spec [:payload :hidden-nodes])
                                :positions (:layout (canvas/read-graph-side-data store grid))})
            {:caption (or (:title spec) (some-> (:graph-name spec) name))
             :href    (str "/view/" (URLEncoder/encode (str grid) "UTF-8") "/data-lineage")})))

(def ^:private model-classes
  [[:types    "Kern"       "np-legend--kern"     "#4a7fc1" "#dbe8f7"]
   [:derived  "Abgeleitet" "np-legend--derived"  "#8a63ad" "#ece1f3"]
   [:use-case "Use Case"   "np-legend--use-case" "#d9942a" "#fdebc8"]])

(def ^:private cardinality
  {:one-to-one "1:1" :one-to-many "1:N" :many-to-one "N:1" :many-to-many "N:N"})

(defn- type-names [schema]
  (into {} (for [[r s] (:object-types schema)]
             [r (some-> (:object-type-name s) name)])))

(defn object-model
  [store schema attrs]
  (let [by-rid (type-names schema)
        specs  (into {} (for [[r s] (:object-types schema)] [(by-rid r) s]))
        nodes  (vec (for [[k label _ line fill] model-classes
                          t (names (get attrs k))]
                      {:id t :klasse label :accent line :fill fill
                       :label (or (some-> (get specs t) :display-name not-empty)
                                  (if (get specs t) t (str t " (fehlt)")))}))
        drawn  (set (map :id nodes))
        edges  (vec (for [[_ l] (:link-types schema)
                          :let [from (get by-rid (:from-object-type l))
                                to   (get by-rid (:to-object-type l))]
                          :when (and (drawn from) (drawn to) (not= from to))]
                      {:source from :target to
                       :label (get cardinality (->kw (:cardinality l)) "")}))]
    (if (empty? nodes)
      "<span class=\"np-missing\">Objektmodell: keine Objekttypen gewählt</span>"
      (figure (draw store nodes edges)
              {:caption "Skizze des Objektmodells"
               :legend  (for [[k label cls] model-classes :when (seq (names (get attrs k)))]
                          [cls label])}))))

(defn- literal [v]
  (cond (string? v) v (keyword? v) (name v) (or (number? v) (boolean? v)) (str v)))

(defn- placeholder? [v] (str/starts-with? v "‹"))

(defn- param-values
  [a p]
  (let [spec (get-in a [:params (->kw p)])]
    (or (seq (keep literal (or (:one-of spec) (get-in spec [:constraints :one-of])
                               (:options spec) (:values spec))))
        (some-> (literal (:default spec)) vector)
        [(str "‹" (name (->kw p)) "›")])))

(defn- targets
  [a t p]
  (for [r     (:rules a)
        :let  [op (->kw (:op r))]
        :when (and (#{:create :update :modify :upsert} op)
                   (= t (->kw (or (:object-type r) (:on a)))))
        v     (let [v (get-in r [:values p])]
                (cond (literal v)                        [(literal v)]
                      (and (vector? v) (= :get (first v))) (param-values a (second v))
                      (some #{p} (map ->kw (:set r)))      (param-values a p)))]
    [v (#{:create :upsert} op)]))

(defn- source-filter
  [a p]
  (reduce
   (fn [acc {:keys [test pred]}]
     (let [vars (set (for [[v path] test
                           :when (and (vector? path) (= :get (first path))
                                      (= p (->kw (peek path))))]
                       (name v)))
           [op x & more] pred
           y    (if (next more) (vec more) (first more))
           [var lit] (if (vars (str x)) [x y] [y x])
           lits (if (coll? lit) (keep literal lit) (keep literal [lit]))]
       (if-not (vars (str var))
         acc
         (case (->kw op)
           (:= :in)         (update acc :only #(if % (into #{} (filter (set lits)) %) (set lits)))
           (:not= :not-in)  (update acc :except (fnil into #{}) lits)
           acc))))
   nil (:submission-criteria a)))

(defn lifecycle
  [store schema {:keys [type property]}]
  (let [t        (->kw type)
        p        (->kw (or (not-empty property) "status"))
        moves    (for [[k a] (:actions schema)
                       :let  [ts (targets a t p)]
                       :when (seq ts)]
                   {:label (or (not-empty (:display-name a)) (some-> (:action-name a) name)
                               (str k))
                    :to    ts
                    :from  (source-filter a p)})
        states   (sort (distinct (concat (for [m moves [v] (:to m)] v)
                                         (for [m moves v (concat (get-in m [:from :only])
                                                                 (get-in m [:from :except]))]
                                           v))))
        any      "jeder Zustand"
        start    "Start"
        edges    (for [{:keys [label to from]} moves
                       [v created?] to
                       f (cond created?      [start]
                               (:only from)  (:only from)
                               (:except from) (remove #(or ((conj (:except from) v) %)
                                                           (placeholder? %))
                                                      states)
                               :else         [any])
                       :when (not= f v)]
                   [[f v] label])
        edges    (vec (for [[[f v] ls] (group-by first edges)]
                        {:source f :target v :label (str/join " · " (distinct (map second ls)))}))
        used     (set (mapcat (juxt :source :target) edges))
        colours  (palette/class-palette states)
        nodes    (vec (concat
                       (for [s states] (merge {:id s :label s :klasse s} (get colours s)))
                       (for [s [start any] :when (used s)]
                         {:id s :label s :klasse s :accent "#9e9e9e" :fill "#f2f2f2"})))]
    (if (empty? moves)
      (str "<span class=\"np-missing\">Lebenszyklus: keine Action setzt "
           (esc (name t)) "." (esc (name p)) "</span>")
      (figure (draw store nodes edges :dot)
              {:caption (str "Lebenszyklus von " (name t) " · geführt in der Eigenschaft "
                             (name p))}))))

(def css
  (str ".np-diagram{margin:8pt 0;break-inside:avoid}"
       ".np-diagram img{max-width:100%;height:auto;display:block}"
       ".np-diagram figcaption{font-size:9pt;color:#5f6b7c;margin-top:4pt}"
       ".np-legend{display:inline-block;padding:0 6px;margin-right:6px;border-radius:3px;"
       "border:1px solid}"
       ".np-legend--kern{background:#dbe8f7;border-color:#4a7fc1}"
       ".np-legend--derived{background:#ece1f3;border-color:#8a63ad}"
       ".np-legend--use-case{background:#fdebc8;border-color:#d9942a}"))
