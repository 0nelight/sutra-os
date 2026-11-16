(ns sutra.capps.layout
  (:require [clojure.string :as str]
            [sutra.capps.walk :as walk]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.format :refer [v]]
            [sutra.capps.bind :as bind]
            [sutra.capps.variables :as vars :refer [deref*]]
            [sutra.capps.visibility :as vis]))

(def container-kinds
  #{:rows :columns :flow :toolbar :section :tabs :pages :loop :profiles :overlay})

(defn layout-node? [x] (and (map? x) (contains? x :kind)))

(defn data-layout?
  [layout]
  (boolean (and (map? layout)
                (or (contains? layout :widget)
                    (contains? container-kinds (some-> (:kind layout) keyword))))))

(def answer-parts
  {"none" #{:descriptor}
   "rows" #{:rows}
   "all"  #{:descriptor :rows}})

(defn answer-wants
  [asked]
  (get answer-parts asked (get answer-parts "all")))

(defn answer-asking
  [parts]
  (some (fn [[v ps]] (when (= ps parts) v)) answer-parts))

(defn legacy-cards?
  [body]
  (and (map? body) (contains? body :cards) (not (contains? body :kind))
       (not (contains? body :widget))))

(defn lower-view-body
  [body]
  (if-not (legacy-cards? body)
    body
    {:kind  :rows
     :items (vec (for [{:keys [widget config title hidden?]} (:cards body)]
                   (cond-> (assoc (dissoc (or config {}) :object-type) :widget widget)
                     (some? title) (assoc :title title)
                     hidden?       (assoc :hidden? true))))}))

(def ^:private shown-param
  {:kpi :value :badge :text})

(defn- item-ref
  [x]
  (when (and (map? x) (= 1 (count x)) (some? (:item x)))
    (->kw (:item x))))

(defn- with-source
  [resolved authored k item]
  (if-let [p (item-ref (get authored k))]
    (assoc resolved ::source {:prop   p
                              :values (v item)
                              :type   (or (:object-type-ref item) (:object-type item))})
    resolved))

(defn- with-sources
  [resolved authored item]
  (let [w (->kw (:widget authored))]
    (cond
      (shown-param w)
      (with-source resolved authored (shown-param w) item)

      (and (= :kpi-row w) (vector? (:items authored)) (vector? (:items resolved)))
      (update resolved :items
              (fn [its] (mapv (fn [r a] (if (map? r) (with-source r a :value item) r))
                              its (:items authored))))

      (and (= :pdf w) (sequential? (:search authored)) (sequential? (:search resolved)))
      (update resolved :search
              (fn [ss] (mapv (fn [r a] (if (map? r) (with-source r a :text item) r))
                             ss (:search authored))))

      (and (= :pdf w) (item-ref (:search authored)))
      (assoc resolved :search
             [(with-source {:text (:search resolved)} authored :search item)])

      :else resolved)))

(defn resolve-item
  [node item]
  (walk/walk-tree
   (fn [x]
     (cond
       (not (map? x))
       nil

       (#{:item :item-not} (bind/form-id x))
       [(vars/resolved x (bind/resolve x (vars/fill-scope :item item)))]

       (and (contains? x :widget) (not (contains? x ::asked)))
       [(let [unanswered (volatile! false)
              resolved   (binding [vars/*read-unanswered* unanswered]
                           (resolve-item (assoc x ::asked true) item))]
          (cond-> (with-sources (dissoc resolved ::asked) x item)
            @unanswered (assoc ::vars/data-pending? true)))]))
   (fn [x k] (= :item (walk/fills-below x k)))
   node))

(defn object-view-node
  [view row {:keys [bound initial-tab tab-var hide-tabs?]}]
  (cond-> (-> (resolve-item view row)
              (dissoc :scenario)
              (update :vars #(apply dissoc % bound)))
    (and initial-tab tab-var (not (contains? (set bound) tab-var)))
    (assoc-in [:vars tab-var :default] initial-tab)

    hide-tabs? (assoc :hide-strip? true)))

(defn fill-arg
  [node arg]
  (if (map? arg)
    (let [scope (vars/fill-scope :arg arg)]
      (walk/postwalk
       (fn [x] (if (= :arg (bind/form-id x)) (vars/resolved x (bind/resolve x scope)) x))
       node))
    node))

(defn loop-groups
  [items group-by]
  (reduce (fn [acc item]
            (let [g (vars/item-field item group-by)]
              (if-let [i (some (fn [[idx [gv _]]] (when (= gv g) idx))
                               (map-indexed vector acc))]
                (update-in acc [i 1] conj item)
                (conj acc [g [item]]))))
          [] items))

(defn active-pane
  [select panes viewer]
  (let [panes* (filterv #(vis/pane-visible? % viewer) panes)
        nm     #(if (or (keyword? %) (symbol? %)) (name %) %)
        sel    (nm (deref* select))
        pane   (or (some #(when (= (nm (:id %)) sel) %) panes*) (first panes*))
        active (:id pane)]
    {:panes panes* :active active :pane pane :body (:body pane)}))

(defn nav-lit
  [nav-ids active prev]
  (cond (contains? nav-ids active) active
        (contains? nav-ids prev)   prev))

(defn nav-duplicate?
  [{:keys [widget on-click]} nav-ids]
  (let [to-nav? (fn [{:keys [do to]}]
                  (and (= :navigate (->kw do)) (contains? nav-ids (some-> to name))))]
    (boolean (and (seq nav-ids) (= :button (->kw widget)) (seq on-click)
                  (every? to-nav? on-click)))))

(defn css-len [x] (if (number? x) (str x "px") x))

(def padding-levels
  {:none [0 0] :compact [16 16] :regular [24 48] :large [40 62]})

(defn padding-sides
  [p]
  (let [level (if (map? p) (:level p) p)]
    (cond
      (some? level)
      (when-let [[y x] (get padding-levels (keyword level))]
        (when (or (pos? y) (pos? x)) [y x y x]))
      (and (map? p) (some p [:top :right :bottom :left]))
      (mapv #(or (get p %) 0) [:top :right :bottom :left]))))

(defn padding-css
  [p]
  (when-let [sides (padding-sides p)]
    (str/join " " (map #(str % "px") sides))))

(defn padding-style
  [p]
  (when-let [[top right] (padding-sides p)]
    {:padding (padding-css p) "--capp-pad-y" (str top "px") "--capp-pad-x" (str right "px")}))

(def header-formats #{:block :contained :floating})
(def border-styles #{:bordered :raised :inset :none})

(defn padding-inner
  [p]
  (when (map? p) (border-styles (some-> (:inner p) keyword))))

(defn header-class
  [h]
  (some->> (some-> h ->kw header-formats) name (str "capp-l-section--")))

(defn border-class
  [b]
  (str "capp-box--border-" (name (or (some-> b ->kw border-styles) :none))))

(defn padding-attrs
  [cls p]
  (let [inner (padding-inner p)
        level (some-> (if (map? p) (:level p) p) name)]
    {:class (cond-> (str cls " capp-pad")
              level                          (str " capp-pad--" level)
              (and inner (not= :none inner)) (str " capp-inner--" (name inner)))
     :style (padding-style p)}))

(def loop-page-max 10000)

(def bg-presets #{:1 :2 :3 :4 :5})

(defn- parse-hex [s] #?(:clj (Integer/parseInt s 16) :cljs (js/parseInt s 16)))

(defn- hex-dark?
  [s]
  (when-let [[_ h] (re-matches #"#?([0-9a-fA-F]{6}|[0-9a-fA-F]{3})" s)]
    (let [h   (if (= 3 (count h)) (apply str (mapcat #(repeat 2 %) h)) h)
          ch  #(/ (parse-hex (subs h % (+ % 2))) 255)
          lum (+ (* 0.2126 (ch 0)) (* 0.7152 (ch 2)) (* 0.0722 (ch 4)))]
      (< lum 0.5))))

(defn- blueprint-dark?
  [s]
  (if-let [[_ fam n] (re-matches #"(dark-gray|light-gray|gray|[a-z]+)([1-5])" s)]
    (case fam "dark-gray" true "light-gray" false "gray" (< (parse-long n) 3)
          (< (parse-long n) 4))
    (= "black" s)))

(defn bg-attrs
  [bg]
  (let [k (some-> bg ->kw)]
    (cond
      (nil? bg)           nil
      (bg-presets k)      {:class (str "capp-l-bg--" (name k))}
      (= :transparent k)  {:style {:background "transparent"}}
      :else (let [s     (name bg)
                  hex?  (str/starts-with? s "#")
                  dark? (if hex? (hex-dark? s) (blueprint-dark? s))]
              {:class (when dark? "bp-dark capp-l-bg--dark")
               :style {:background (if hex? s (str "var(--" s ")"))}}))))

(defn module-title
  [layout app-label]
  (or (not-empty (str (get-in layout [:header :title] "")))
      (not-empty (str (:title layout "")))
      app-label))

(defn column-box
  [{:keys [w min-w max-w h min-h max-h grow? flex]}]
  (let [flex (or flex (when grow? 1))
        w    (when-not flex w)]
    {:modifiers (cond-> [] w (conj :w) flex (conj :flex))
     :style     (cond-> nil
                  w                   (assoc "--capp-col-w" (css-len w))
                  flex                (assoc "--capp-flex" flex)
                  (and min-w (not w)) (assoc :min-width (css-len min-w))
                  (and max-w (not w)) (assoc :max-width (css-len max-w))
                  h                   (assoc :height (css-len h))
                  (and min-h (not h)) (assoc :min-height (css-len min-h))
                  (and max-h (not h)) (assoc :max-height (css-len max-h)
                                             :overflow "auto"))}))

(defn sized-box?
  [{:keys [w min-w max-w h min-h max-h grow? flex]}]
  (boolean (or w min-w max-w h min-h max-h grow? flex)))

(defn framed-box?
  [{:keys [icon help border]}]
  (boolean (or icon help (and border (not= :none (->kw border))))))

(defn box-style
  [{:keys [w min-w max-w h min-h max-h grow? flex]}]
  (cond-> {}
    w             (assoc :width (css-len w))
    min-w         (assoc :min-width (css-len min-w))
    max-w         (assoc :max-width (css-len max-w))
    h             (assoc :height (css-len h))
    min-h         (assoc :min-height (css-len min-h))
    max-h         (assoc :max-height (css-len max-h) :overflow "auto")
    (or flex grow?) (assoc "--capp-flex" (or flex 1))))

(defn split-step
  [{:keys [open? tapped?]} open-now?]
  (let [opened? (and open-now? (not open?))]
    {:state   {:open? open-now? :tapped? (boolean (and open-now? (or opened? tapped?)))}
     :reveal? opened?}))

(defn split-back
  [tapped? pushes?]
  (if (and tapped? pushes?) :history-back :clear))

(defn pick-variant
  [select variants default]
  (let [active (deref* select)]
    (or (some #(when (= (:id %) active) %) variants)
        (some #(when (= (:id %) default) %) variants)
        (first variants))))
