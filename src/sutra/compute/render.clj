(ns sutra.compute.render
  (:require [sutra.kit.refusal :as refusal]
            [sutra.compute.ports :as ports]
            [sutra.kit.time :as time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.lookup :as lookup]
            [sutra.compute.external :as ext]
            [sutra.ref.term :as term]
            [sutra.types.template :as tmpl]
            [sutra.viz.graph-layout :as gl]
            [sutra.viz.graph-svg :as gsvg]
            [sutra.viz.palette :as palette]
            [sutra.viz.layouts :as layouts]
            [sutra.viz.page :as page]))

(set! *warn-on-reflection* true)

(def ^:private default-max-objects 500)

(def ^:private driver
  "import sutra_render\nsutra_render.main()\n")

(defn- type-name-of
  [schema rid]
  (or (get-in schema [:object-types rid :object-type-name])
      (get-in schema [:interfaces rid :interface-name])
      (when (keyword? rid) rid)))

(defn- zoned
  [^java.time.Instant i]
  (str (.toOffsetDateTime (.atZone i (java.time.ZoneId/systemDefault)))))

(defn- printable
  [v]
  (condp instance? v
    java.sql.Timestamp  (zoned (time/stored-timestamp->instant v))
    java.sql.Date       (str (time/sql-date->local-date v))
    java.time.Instant   (zoned v)
    java.util.Date      (str (.toInstant ^java.util.Date v))
    v))

(defn- row->map
  [row]
  (let [{:keys [id values]} row]
    (cond-> (update-vals (if (map? values) values (dissoc row :id)) printable)
      id (assoc :id id))))

(defn- ->number
  [v]
  (cond
    (number? v) v
    (string? v) (try (Double/parseDouble (str/trim v))
                     (catch NumberFormatException _
                       (refusal/refuse! :refusal.foreign/not-a-number {} {:value v})))
    :else (refusal/refuse! :refusal.foreign/not-a-number {} {:value v})))

(defn- read-objects
  [store schema spec limit]
  (->> ((:query (ports/object-reads store)) store schema (assoc spec :limit limit))
       :rows
       (mapv row->map)))

(defn- object-set-spec
  [find v]
  (cond
    (nil? v)        {:find find}
    (sequential? v) {:find find :ids (vec v)}
    (map? v)        (merge {:find find} (select-keys v [:ids :object-set :object-set-args
                                                        :where :order-by :sort-dir]))
    :else (refusal/refuse! :refusal.foreign/object-set-value {} {:value v})))

(defn- read-object!
  [store schema pname rid raw]
  (let [{:keys [object problem]} ((:resolve (ports/object-reads store)) store schema rid raw)]
    (when problem
      (refusal/refuse! (if (= :unresolved (:reason problem))
                       :refusal.compute/template-parameter-unresolved
                       :refusal.compute/template-parameter-problem)
                     (assoc (dissoc problem :message) :parameter pname)
                     {:message (:message problem)}))
    (row->map object)))

(defn- linked-spec
  [parameters pname anchor-pname anchor anchor-type link]
  (when (tmpl/traversal-anchor-problem parameters pname)
    (let [{:keys [search-around of]} (get parameters pname)]
      (refusal/refuse! :refusal.compute/traversal-anchor-not-object
                       {:parameter pname :search-around search-around :of of})))
  (when-not anchor
    (refusal/refuse! :refusal.foreign/anchorless-traversal {:parameter pname}
                     {:link link :anchor anchor-pname}))
  {:find anchor-type :ids [(str (:id anchor))]
   :around (tmpl/search-around-sides link)})

(defn- graph-nodes
  [schema rid rows]
  (let [title (get-in schema [:object-types rid :title-key])
        klass (name (or (lookup/name-of schema :object-types rid) rid))]
    (mapv (fn [row]
            {:id     (str (:id row))
             :klasse klass
             :label  (or (when title (not-empty (str (get row title))))
                         (str (:id row)))})
          rows)))

(defn- rid-of-type
  [schema t]
  (let [find (type-name-of schema t)]
    [find (or (lookup/rid-of schema :object-types find) t)]))

(defn- graph-edges
  [store schema pname rids edges ids]
  (let [connected (:connected (ports/object-reads store))
        walked (for [[find rid] rids, e edges]
                 [e (try {:ok (connected (sub/reader store) schema rid e ids)}
                         (catch clojure.lang.ExceptionInfo ex
                           {:no (str/replace (str (.getMessage ex)) (str rid) (str find))}))])
        found  (into #{} (keep (fn [[e r]] (when (:ok r) e))) walked)]
    (when-let [[e r] (first (remove (comp found first) walked))]
      (refusal/refuse! :refusal.compute/template-parameter-problem
                       {:parameter pname :edge e} {:message (:no r)}))
    (vec (distinct (for [[_ {:keys [ok]}] walked
                         [source targets] ok
                         target targets]
                     {:source (str source) :target (str target)})))))

(defn- boxed
  [nodes wide]
  (mapv (fn [n]
          (let [[w h] (gl/label-box n wide)]
            (assoc n :w w :h h :umbruch wide)))
        nodes))

(defn- spread
  [placed]
  (when (seq placed)
    [(- (apply max (map #(+ (or (gl/->num (:x %)) 0) (or (gl/->num (:w %)) 0)) placed))
        (apply min (map #(or (gl/->num (:x %)) 0) placed)))
     (- (apply max (map #(+ (or (gl/->num (:y %)) 0) (or (gl/->num (:h %)) 0)) placed))
        (apply min (map #(or (gl/->num (:y %)) 0) placed)))]))

(defn- best-placement
  [store nodes edges asked sheet]
  (let [place (fn [wide] (layouts/place (ports/viz-layouts store) (boxed nodes wide) edges asked))
        [bw bh] [(or (gl/->num (first sheet)) 0) (or (gl/->num (second sheet)) 0)]]
    (if-not (and (pos? bw) (pos? bh))
      (place gl/label-wrap-px)
      (->> gl/label-wraps
           (map place)
           (apply max-key (fn [p]
                            (let [[w h] (or (spread p) [0 0])]
                              (if (and (pos? w) (pos? h)) (min (/ bw w) (/ bh h)) 0.0))))))))

(defn- read-graph
  [store schema max-objects pname {:keys [object-type edge layout richtung]} override sheet]
  (let [rids    (mapv #(rid-of-type schema %)
                      (if (sequential? object-type) object-type [object-type]))
        sides   (vec (if (sequential? edge) edge [edge]))
        nodes   (vec (mapcat (fn [[find rid]]
                               (graph-nodes schema rid
                                            (read-objects store schema {:find find} max-objects)))
                             rids))
        classes (palette/class-palette (map :klasse nodes))
        nodes   (mapv #(merge % (get classes (:klasse %))) nodes)
        edges   (graph-edges store schema pname rids sides (mapv :id nodes))
        asked   (cond-> {:layout (or override layout)}
                  richtung (assoc :richtung richtung))]
    (gsvg/graph->svg {:nodes (best-placement store nodes edges asked sheet)
                      :edges edges
                      :fit   sheet})))

(defn- bind-one
  [store schema max-objects pname {:keys [type object-type required] :as pspec} raw layout sheet]
  (let [find (type-name-of schema object-type)]
    (cond
      (and (nil? raw) required)
      (refusal/refuse! :refusal.foreign/parameter-required {:parameter pname})

      (and (nil? raw) (not= :graph type))
      (when (= :object-set type) [])

      :else
      (case type
        :string  (str raw)
        :number  (->number raw)
        :boolean (let [b (:value (term/parse [:scalar :boolean :boolean] raw))]
                   (if (boolean? b)
                     b
                     (refusal/refuse! :refusal.foreign/not-a-boolean {} {:value raw})))
        (:date :timestamp) (str raw)
        :object  (read-object! store schema pname object-type (str raw))
        :object-set (read-objects store schema (object-set-spec find raw) max-objects)
        :graph   (read-graph store schema max-objects pname pspec layout sheet)
        (refusal/refuse! :refusal.foreign/unknown-parameter-type {:parameter pname}
                         {:type type})))))

(defn- raw-of
  [pspec values pname preview?]
  (let [supplied (get values pname)]
    (cond
      (some? supplied) supplied
      preview?         (or (:preview pspec) (:default pspec))
      :else            (:default pspec))))

(defn bind
  ([store schema template values preview?] (bind store schema template values preview? nil))
  ([store schema {:keys [parameters limits page]} values preview? layout]
   (let [max-objects (or (:max-objects limits) default-max-objects)
         sheet       (page/content-box-px page)
         linked?     (fn [[_ pspec]] (some? (:search-around pspec)))
         anchors     (reduce (fn [acc [pname pspec]]
                               (assoc acc pname
                                      (bind-one store schema max-objects pname pspec
                                                (raw-of pspec values pname preview?)
                                                layout sheet)))
                             {}
                             (remove linked? parameters))]
     (into {:jetzt (str (java.time.OffsetDateTime/now))}
           (reduce (fn [acc [pname {:keys [search-around of]}]]
                     (let [anchor-spec (get parameters of)
                           anchor-type (when (= :object (:type anchor-spec))
                                         (type-name-of schema (:object-type anchor-spec)))]
                       (assoc acc pname
                              (read-objects
                               store schema
                               (linked-spec parameters pname of (get anchors of) anchor-type
                                            search-around)
                               max-objects))))
                   anchors
                   (filter linked? parameters))))))

(defn- render-in-jail!
  [job seconds]
  (let [out (ext/run-python! {:label       :template-render
                              :code        driver
                              :profile     (when (and seconds (> seconds 30)) :large)
                              :stdin       (json/write-str job)})
        res (try (json/read-str out :key-fn keyword)
                 (catch Exception _
                   (refusal/refuse! :refusal.compute/render-no-result
                                    {:raw (subs (str out) 0 (min 400 (count (str out))))})))]
    (if (:ok res)
      (dissoc res :ok)
      (refusal/refuse! :refusal.compute/render-failed
                       {:detail (:error res)
                        :template (:template res) :line (:line res)}))))

(defn- links-of
  [schema parameters ctx]
  (into []
        (comp (mapcat (fn [[pname {:keys [type object-type]}]]
                        (let [tn (some-> (type-name-of schema object-type) name)
                              v  (get ctx pname)]
                          (when tn
                            (case type
                              :object     (when (:id v) [{:object-type tn :id (str (:id v))}])
                              :object-set (for [r v :when (:id r)]
                                            {:object-type tn :id (str (:id r))})
                              nil)))))
              (distinct))
        parameters))

(defn- drawn-parameters
  [parameters]
  (vec (sort (for [[pname {:keys [type]}] parameters :when (= :graph type)] (name pname)))))

(defn generate
  [template {:keys [store schema values targets preview? literal? layout] :or {targets [:html]}}]
  (when-not (:source template)
    (refusal/refuse! :refusal.foreign/template-no-body {} {:template (:template-name template)}))
  (let [ctx (when-not literal?
              (bind store schema template values preview? layout))]
    (cond-> (render-in-jail!
             {:name     (some-> (:template-name template) name)
              :source   (:source template)
              :format   (or (:format template) :html)
              :targets  (mapv name targets)
              :page     (:page template)
              :literal  (boolean literal?)
              :raw-html (drawn-parameters (:parameters template))
              :context  (or ctx {})}
             (:seconds (:limits template)))
      (not literal?) (assoc :links (links-of schema (:parameters template) ctx)))))
