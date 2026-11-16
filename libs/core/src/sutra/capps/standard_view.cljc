(ns sutra.capps.standard-view
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.tree :as tree]
            [clojure.string :as str]
            [sutra.capps.table :as tbl]
            [sutra.capps.format :refer [v]]
            [sutra.capps.variables :refer [link-key row-prop view-object view-objects]]
            [sutra.kit.remote :as rd]))

(defn- display-entries
  [display]
  (->> (vals display)
       (filter #(and (map? %) (:prop %)))
       (reduce (fn [m e] (update m (->kw (:prop e)) #(or % e))) {})
       vals
       (sort-by #(let [o (:order %)]
                   (cond (number? o) o
                         (string? o) (or #?(:clj (parse-long o) :cljs (let [n (js/parseInt o 10)]
                                                                        (when-not (js/isNaN n) n)))
                                         1e9)
                         :else       1e9)))
       vec))

(defn- item->spec
  [x]
  (cond
    (map? x)                        (update x :prop ->kw)
    (or (keyword? x) (string? x))   {:prop (->kw x)}
    :else                           nil))

(defn- blank-value?
  [x]
  (or (nil? x) (and (string? x) (str/blank? x))))

(defn property-entries
  [row display {:keys [props only-prominent? exclude hide-null?]} ctx]
  (when row
    (let [entries (display-entries display)
          by-prop (coll/index-by (comp ->kw :prop) entries)
          excl    (into #{} (keep ->kw) exclude)
          specs   (if (seq props)
                    (keep item->spec props)
                    (->> entries
                         (remove #(= :hidden (->kw (:visibility %))))
                         (filter #(or (not only-prominent?)
                                      (= :prominent (->kw (:visibility %)))))
                         (map (fn [e] {:prop (->kw (:prop e))}))
                         (remove #(contains? excl (:prop %)))))]
      (into []
            (keep (fn [{:keys [prop label fmt]}]
                    (let [e     (get by-prop prop)
                          d     (tbl/prop-look e prop (v row))
                          value (get (v row) prop)]
                      (when-not (and hide-null? (blank-value? value))
                        (cond-> {:prop       prop
                                 :label      (or (not-empty (str label)) (:label d))
                                 :text       (when-not (blank-value? value)
                                               (str ((if fmt (tbl/fmt-by fmt) (:fmt d))
                                                     value ctx)))
                                 :prominent? (= :prominent (->kw (:visibility e)))}
                          (:look d) (assoc :look (:look d)))))))
            specs))))

(defn prominent-props
  [display]
  (into [] (comp (filter #(= :prominent (->kw (:visibility %))))
                 (map (comp ->kw :prop)))
        (display-entries display)))

(defn subtitle-text
  [row display prop ctx]
  (when-let [prop (->kw prop)]
    (let [value (row-prop row prop)]
      (when-not (blank-value? value)
        (let [e (some #(when (= prop (->kw (:prop %))) %) (display-entries display))
              d (tbl/prop-display e prop)]
          (str ((tbl/fmt-by (or (:fmt d) :auto)) value ctx)))))))

(defn placed-actions
  [tree]
  (into #{}
        (comp (filter #(and (contains? % :widget) (some? (:action %))))
              (keep #(->kw (:action %))))
        (tree/nodes tree)))

(defn- state-gate
  [t property from]
  (let [cur {:object-property (->kw property) :of t}]
    (into [:or] (for [s from] [:= cur (str s)]))))

(defn- action-items
  [t actions skip]
  (vec (for [{:keys [action label property from]} actions
             :let [an    (->kw action)
                   label (or (not-empty (str label)) (name an))]
             :when (not (contains? skip an))]
         (cond-> {:kind  :section
                  :title label
                  :body  {:widget :action-form :action an :label label}}
           (and property (seq from)) (assoc :when (state-gate t property from))))))

(defn link-sections
  [links]
  (vec (for [{:keys [api label cols]} links]
         {:kind  :section
          :title (or label (name api))
          :class (str "capp-l-linkside capp-l-linkside--" (name api))
          :when  {:from (link-key api)}
          :body  {:widget       :object-table
                  :rows         {:from (link-key api)}
                  :cols         (vec (for [c cols] {:prop (->kw c)}))
                  :sort         (vec (take 1 cols))
                  :auto-active? false}})))

(def default-texts
  {:properties "Eigenschaften"
   :actions   "Aktionen"
   :no-action "_In diesem Stand ist keine Handlung möglich._"
   :location  "Standort"
   :maps-link "In Google Maps öffnen"})

(defn geo-props
  [display]
  (into [] (comp (filter #(and (= :prominent (->kw (:visibility %)))
                               (= :geopoint (->kw (:type %)))))
                 (map (comp ->kw :prop)))
        (display-entries display)))

(defn- location-section
  [t txt g]
  {:kind  :section
   :title (:location txt)
   :class "capp-l-location"
   :when  {:object-property g :of t}
   :body  {:kind  :rows
           :items [{:widget :map
                    :height 220
                    :zoom   16
                    :auto-zoom? true
                    :layers [{:rows       {:val view-object}
                              :geometries [{:geometry :point :geopoint g}]}]}
                   {:widget   :hyperlink
                    :label    (:maps-link txt)
                    :url-type :templated
                    :template (str "https://www.google.com/maps/search/?api=1&query={"
                                   (name g) "}")
                    :object   {:val view-object}
                    :open-in  :new-tab}]}})

(declare node)

(defn subject
  [{:keys [asked resolved catalog id entry]}]
  (cond
    (nil? asked)
    {:show :none}

    (nil? resolved)
    (rd/fold catalog {:loading (constantly {:show :loading})
                      :absent  (constantly {:show :unknown-type})
                      :failed  (fn [e] {:show :failed :error e})
                      :value   (constantly {:show :unknown-type})})

    (or (nil? id) (nil? entry) (and (:loading? entry) (nil? (:instance entry))))
    {:show :loading}

    (:error entry)            {:show :failed :error (:error entry)}
    (nil? (:instance entry))  {:show :not-found}
    :else                     {:show :page}))

(defn page-view
  [object-type facts display opts]
  (let [own  (get-in facts [:object-page :view])
        info (:standard-view facts)]
    (cond
      own   {:source :own :view own}
      info  {:source :standard :view (node object-type info display opts)}
      :else {:source :none})))

(defn node
  ([t info display] (node t info display nil))
  ([t {:keys [links actions]} display {:keys [skip] :as opts}]
   (let [t    (->kw t)
         txt  (merge default-texts (:texts opts))
         prom (prominent-props display)
         acts (when (get opts :actions? true)
                (action-items t actions (into #{} (keep ->kw) skip)))]
     {:kind  :rows
      :items (cond-> []
               (seq (remove (set (geo-props display)) prom))
               (conj {:widget :property-cards
                      :object {:val view-object}
                      :cards  (vec (remove (set (geo-props display)) prom))})

               true
               (into (map #(location-section t txt %)) (geo-props display))

               true
               (conj {:kind  :section
                      :title (:properties txt)
                      :class "capp-l-props"
                      :body  {:widget     :property-list
                              :object     {:val view-object}
                              :exclude    prom
                              :columns    2
                              :hide-null? true}})

               (seq links)
               (into (link-sections links))

               (seq acts)
               (conj {:kind  :section
                      :title (:actions txt)
                      :body  {:kind  :rows
                              :items (let [gates (keep :when acts)]
                                       (cond-> acts
                                         (= (count gates) (count acts))
                                         (conj {:widget :markdown
                                                :text   (:no-action txt)
                                                :when   [:not (into [:or] gates)]})))}}))})))

(def ^:private set-chart-max 5)
(def ^:private set-list-max 3)

(def ^:private categorical-types #{:string :boolean})

(defn- hints [e] (into #{} (keep ->kw) (:render-hints e)))

(defn set-groups
  [display]
  (let [shown  (remove #(= :hidden (->kw (:visibility %))) (display-entries display))
        hinted (filter #(some (hints %) [:low-cardinality :selectable]) shown)
        prom   (filter #(and (= :prominent (->kw (:visibility %)))
                             (contains? categorical-types (->kw (:type %))))
                       shown)]
    (into [] (comp (map (comp ->kw :prop)) (distinct) (take set-chart-max))
          (concat hinted prom))))

(defn set-cols
  [display title-key]
  (into [] (comp (remove nil?) (map ->kw) (distinct) (take set-list-max))
        (cons title-key (prominent-props display))))

(def default-set-texts
  {:charts "Diagramme" :list "Liste"})

(defn set-node
  ([display] (set-node display nil))
  ([display {:keys [title-key texts]}]
   (let [txt    (merge default-set-texts texts)
         label  #(:label (tbl/prop-display (some (fn [e] (when (= % (->kw (:prop e))) e))
                                                 (display-entries display))
                                           %))
         groups (set-groups display)
         cols   (set-cols display title-key)
         lst    (cond-> {:widget       :object-table
                         :rows         {:val view-objects}
                         :auto-active? false
                         :on-row-click {:do :open-object}}
                  (seq cols) (assoc :cols (mapv (fn [c] {:prop c}) cols)))]
     (if (empty? groups)
       lst
       {:kind   :tabs
        :vars   {:set-tab {:default :charts}}
        :select {:var :set-tab}
        :tabs   [{:id :charts :label (:charts txt)
                  :body {:kind  :rows
                         :items (vec (for [g groups]
                                       {:kind  :section
                                        :title (or (label g) (name g))
                                        :body  {:widget     :chart
                                                :rows       {:val view-objects}
                                                :x          g
                                                :y          g
                                                :agg        :count
                                                :chart-kind :bar
                                                :height     180}}))}}
                 {:id :list :label (:list txt) :body lst}]}))))
