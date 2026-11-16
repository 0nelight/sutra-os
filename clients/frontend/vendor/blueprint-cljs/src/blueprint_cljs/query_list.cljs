(ns blueprint-cljs.query-list
  "QueryList (Blueprint select package): the engine under Select, Suggest, MultiSelect and
   Omnibar — filters `:items` by a query, tracks the active item, handles ↑/↓/Enter, and
   hands everything to a `:renderer`. Items are any values; equality is `=` unless
   `:items-equal` (fn or key) says otherwise."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [blueprint-cljs.menu :as menu]))

;; ---------------------------------------------------------------- shared helpers

(def create-new-item
  "The active item while the \"create new item\" entry is active."
  ::create-new-item)

(defn get-create-new-item [] create-new-item)

(defn create-new-item? [x] (= x create-new-item))

(defn get-active-item
  "The active item, nil while it is the create-new entry."
  [x]
  (when-not (create-new-item? x) x))

(defn items-equal?
  "Blueprint `executeItemsEqual`: `items-equal` is nil (=), a fn or a key."
  [items-equal a b]
  (cond
    (or (nil? items-equal) (nil? a) (nil? b)) (= a b)
    (fn? items-equal)                         (boolean (items-equal a b))
    :else                                     (= (get a items-equal) (get b items-equal))))

(defn render-filtered-items
  "The menu content for `list-props` (what an `:item-list-renderer` gets): the
   `:initial-content` while the query is empty (when the opts carry that key), else the
   rendered filtered items, else `:no-results`."
  [{:keys [query filtered-items render-item]} opts]
  (if (and (str/blank? query) (contains? opts :initial-content))
    (:initial-content opts)
    (let [items (doall (keep-indexed (fn [i item] (render-item item i)) filtered-items))]
      (if (seq items) items (:no-results opts)))))

(defn- escape-re [s]
  (str/replace s #"([.*+?^=!:${}()|\[\]/\\])" "\\$1"))

(defn highlight-text
  "`text` as a seq (not a vector: hiccup would read it as an element) of strings and
   [:strong] for each match of a word of `query`."
  [text query]
  (let [words (->> (str/split (str query) #"\s+") (remove str/blank?) (map escape-re))]
    (if (empty? words)
      (list text)
      (let [re (js/RegExp. (str/join "|" words) "gi")]
        (loop [last 0 tokens []]
          (if-let [m (.exec re text)]
            (let [len    (count (aget m 0))
                  end    (.-lastIndex re)
                  before (subs text last (- end len))]
              (if (zero? len)
                (do (set! (.-lastIndex re) (inc end)) (recur last tokens))
                (recur end (cond-> tokens
                             (seq before) (conj before)
                             true         (conj ^{:key end} [:strong (aget m 0)])))))
            (seq (cond-> tokens (< last (count text)) (conj (subs text last))))))))))

(defn item-disabled?
  [item index item-disabled]
  (cond
    (or (nil? item-disabled) (nil? item)) false
    (fn? item-disabled)                   (boolean (item-disabled item index))
    :else                                 (boolean (get item item-disabled))))

(defn- wrap-number [v lo hi]
  (cond (< v lo) hi (> v hi) lo :else v))

(defn get-first-enabled-item
  "Blueprint `getFirstEnabledItem`: from `start` in `direction`, wrapping around."
  ([items item-disabled] (get-first-enabled-item items item-disabled 1 (dec (count items))))
  ([items item-disabled direction start]
   (let [items (vec items) mx (dec (count items))]
     (when (seq items)
       (loop [i start]
         (let [n (wrap-number (+ i direction) 0 mx)]
           (cond
             (not (item-disabled? (nth items n) n item-disabled)) (nth items n)
             (or (= n start) (= start -1))                       nil
             :else                                                (recur n))))))))

(defn filtered-items
  [query {:keys [items item-predicate item-list-predicate]}]
  (vec (cond
         (fn? item-list-predicate) (item-list-predicate query items)
         (fn? item-predicate)      (keep-indexed (fn [i x] (when (item-predicate query x i false)
                                                             x))
                                                 items)
         :else                     items)))

(defn- matching-item [query {:keys [items item-predicate]}]
  (when (fn? item-predicate)
    (some (fn [[i x]] (when (item-predicate query x i true) x)) (map-indexed vector items))))

(defn keyboard-click? [e] (contains? #{"Enter" " "} (.-key e)))

(defn target-self-activates-on-key-up?
  "Blueprint `targetSelfActivatesOnKeyUp`: a button (or link, for Enter) inside the
   target fires its own click, so the key handler must not toggle as well."
  [e]
  (let [t (.-target e)]
    (boolean
     (when (instance? js/Element t)
       (let [sel (if (= " " (.-key e)) "button, .bp-button" "button, a[href], .bp-button")
             m   (.closest t sel)]
         (when (and m (.contains (.-currentTarget e) m))
           (not (.matches m ":disabled, .bp-disabled"))))))))

;; ---------------------------------------------------------------- QueryList

(defonce ^:private id-counter (atom 0))

(defn- px [s] (or (some-> s (js/parseInt 10)) 0))

(def ^:private defaults {:disabled false :reset-on-query true})

(defn query-list
  "Props (Blueprint ListItemsProps + QueryListProps, kebab-cased): `:items`,
   `:item-renderer (fn [item {:keys [handle-click handle-focus id index modifiers query
   ref]}])` with `:modifiers {:active :disabled :matches-predicate}`, `:item-predicate
   (fn [query item index exact-match?])`, `:item-list-predicate (fn [query items])`,
   `:item-list-renderer (fn [{:keys [active-item filtered-items items query
   items-parent-ref menu-props render-item render-create-item]}])`, `:items-equal`,
   `:item-disabled` (key or fn [item index]), `:active-item` (controlled) /
   `:initial-active-item`, `:on-active-item-change (fn [item create-new?])`,
   `:on-item-select (fn [item event])`, `:on-items-paste`, `:on-query-change (fn [query
   event])`, `:query` (controlled), `:create-new-item-from-query`,
   `:create-new-item-renderer (fn [query active? handle-click])`,
   `:create-new-item-position` :first|:last, `:initial-content`, `:no-results`,
   `:reset-on-query` (true), `:reset-on-select`, `:scroll-to-active-item` (true),
   `:menu-props`, `:on-key-down`, `:on-key-up`, `:list-id`, `:disabled`, `:class`,
   `:api-ref (fn [{:keys [set-query! set-active-item! scroll-active-item-into-view!]}])`,
   and `:renderer (fn [{:keys [active-item active-item-id filtered-items query list-id
   class item-list handle-item-select handle-key-down handle-key-up handle-paste
   handle-query-change]}])`."
  [init]
  (let [init      (merge defaults init)
        list-id   (or (:list-id init) (str "bp-query-list-" (swap! id-counter inc)))
        query0    (or (:query init) "")
        fitems0   (filtered-items query0 init)
        state     (r/atom {:query          query0
                           :filtered-items fitems0
                           :create-new     (some-> (:create-new-item-from-query init)
                                                   (apply [query0]))
                           :active-item    (if (contains? init :active-item)
                                             (:active-item init)
                                             (or (:initial-active-item init)
                                                 (get-first-enabled-item
                                                  fitems0 (:item-disabled init))))})
        cur       (atom init)
        parent    (atom nil)
        item-refs (atom {})
        check-vp? (atom false)
        expected  (atom nil)
        enter?    (atom false)
        props     (fn [] @cur)]
    (letfn [(can-create? []
              (let [p (props)]
                (and (some? (:create-new-item-from-query p))
                     (some? (:create-new-item-renderer p)))))
            (create-first? [] (= :first (keyword (:create-new-item-position (props)))))
            (would-match-existing? [cn]
              (let [cns (if (sequential? cn) cn [cn])
                    eq  (:items-equal (props))]
                (some (fn [item] (some #(items-equal? eq item %) cns))
                      (:filtered-items @state))))
            (create-rendered? [cn]
              (and (can-create?)
                   (not= "" (:query @state))
                   (not (would-match-existing? cn))))
            (active-index
              ([] (active-index (:filtered-items @state)))
              ([items]
               (let [a (:active-item @state) eq (:items-equal (props))]
                 (if (or (nil? a) (create-new-item? a))
                   -1
                   (or (first (keep-indexed (fn [i x] (when (items-equal? eq x a) i)) items))
                       -1)))))
            (set-active-item! [item]
              (reset! expected item)
              (when-not (contains? (props) :active-item)
                (reset! check-vp? true)
                (swap! state assoc :active-item item))
              (when-let [f (:on-active-item-change (props))]
                (if (create-new-item? item) (f nil true) (f item false))))
            (set-query!
              ([q] (set-query! q (:reset-on-query (props)) (props) nil))
              ([q reset-active] (set-query! q reset-active (props) nil))
              ([q reset-active p e]
               (reset! check-vp? true)
               (when (and (not= q (:query @state)) (:on-query-change p))
                 ((:on-query-change p) q e))
               (let [tq    (str/trim q)
                     fi    (filtered-items tq p)
                     cnf   (:create-new-item-from-query p)
                     cn    (when (and cnf (not= tq "")) (cnf tq))
                     _     (swap! state assoc :create-new cn :filtered-items fi :query q)
                     idx   (active-index fi)
                     reset (or reset-active (neg? idx)
                               (item-disabled? (get-active-item (:active-item @state)) idx
                                               (:item-disabled p)))]
                 (when reset
                   (if (and (create-rendered? cn) (create-first?))
                     (set-active-item! create-new-item)
                     (set-active-item! (get-first-enabled-item fi (:item-disabled p))))))))
            (active-element []
              (when-let [pe @parent]
                (let [a (:active-item @state)]
                  (if (create-new-item? a)
                    (.item (.-children pe) (if (create-first?) 0
                                               (count (:filtered-items @state))))
                    (let [i (active-index)]
                      (or (get @item-refs i) (.item (.-children pe) i)))))))
            (scroll-active-into-view! []
              (let [p        (props)
                    external (not (items-equal? (:items-equal p) (get-active-item @expected)
                                                (get-active-item (:active-item p))))]
                (reset! expected nil)
                (when-not (and (false? (:scroll-to-active-item p)) external)
                  (let [el (active-element) pe @parent]
                    (when (and pe el)
                      (let [cs     (js/getComputedStyle pe)
                            pt     (px (.-paddingTop cs))
                            pb     (px (.-paddingBottom cs))
                            top    (.-offsetTop el)
                            height (.-offsetHeight el)
                            ptop   (.-offsetTop pe)
                            bottom-edge (- (+ top height pb) ptop)
                            top-edge    (- top pt ptop)]
                        (cond
                          (>= bottom-edge (+ (.-scrollTop pe) (.-clientHeight pe)))
                          (set! (.-scrollTop pe) (- (+ bottom-edge height) (.-clientHeight pe)))
                          (<= top-edge (.-scrollTop pe))
                          (set! (.-scrollTop pe) (- top-edge height)))))))))
            (maybe-reset-query! []
              (when (:reset-on-select (props)) (set-query! "" true)))
            (handle-item-create! [q e]
              (when-let [v (some-> (:create-new-item-from-query (props)) (apply [q]))]
                (doseq [item (if (sequential? v) v [v])]
                  (when-let [f (:on-item-select (props))] (f item e)))
                (maybe-reset-query!)))
            (handle-item-select [item e]
              (set-active-item! item)
              (when-let [f (:on-item-select (props))] (f item e))
              (maybe-reset-query!))
            (handle-paste [queries]
              (let [p (props)
                    {:keys [next-active emit next-queries]}
                    (reduce (fn [acc q]
                              (if-let [m (matching-item q p)]
                                (-> acc (assoc :next-active m) (update :emit conj m))
                                (if (can-create?)
                                  (let [v ((:create-new-item-from-query p) q)]
                                    (cond-> acc
                                      (some? v) (update :emit into
                                                        (if (sequential? v) v [v]))))
                                  (update acc :next-queries conj q))))
                            {:next-active nil :emit [] :next-queries []}
                            queries)]
                (set-query! (str/join ", " next-queries) false)
                (when (some? next-active) (set-active-item! next-active))
                (when-let [f (:on-items-paste p)] (f emit))))
            (next-active-item [direction]
              (let [start (active-index)
                    fi    (:filtered-items @state)]
                (if (and (create-rendered? (:create-new @state))
                         (or (and (zero? start) (= direction -1))
                             (and (= start (dec (count fi))) (= direction 1))))
                  create-new-item
                  (get-first-enabled-item fi (:item-disabled (props)) direction start))))
            (handle-key-down [^js e]
              (when-not (.. e -nativeEvent -isComposing)
                (case (.-key e)
                  ("ArrowUp" "ArrowDown")
                  (do (.preventDefault e)
                      (when-let [n (next-active-item (if (= "ArrowUp" (.-key e)) -1 1))]
                        (set-active-item! n)))
                  "Enter" (reset! enter? true)
                  nil))
              (when-let [f (:on-key-down (props))] (f e)))
            (handle-key-up [e]
              (when (and (= "Enter" (.-key e)) @enter?)
                (.preventDefault e)
                (let [a (:active-item @state)]
                  (if (or (nil? a) (create-new-item? a))
                    (handle-item-create! (:query @state) e)
                    (handle-item-select a e)))
                (reset! enter? false))
              (when-let [f (:on-key-up (props))] (f e)))
            (handle-query-change [e]
              (let [q (if e (.. e -target -value) "")]
                (set-query! q (:reset-on-query (props)) (props) e)))
            (render-item [item index]
              (let [p (props)]
                (when-not (true? (:disabled p))
                  (let [{:keys [active-item query filtered-items]} @state
                        eq  (:items-equal p)
                        el  ((:item-renderer p)
                             item
                             {:handle-click #(handle-item-select item %)
                              :handle-focus #(set-active-item! item)
                              :id           (str list-id "-item-" index)
                              :index        index
                              :modifiers    {:active (items-equal? eq (get-active-item
                                                                       active-item)
                                                                   item)
                                             :disabled (item-disabled? item index
                                                                       (:item-disabled p))
                                             :matches-predicate
                                             (boolean (some #(items-equal? eq % item)
                                                            filtered-items))}
                              :query        query
                              :ref          (fn [node]
                                              (if node
                                                (swap! item-refs assoc index node)
                                                (swap! item-refs dissoc index))
                                              nil)})]
                    (if (and (vector? el) (nil? (:key (meta el))))
                      (with-meta el (assoc (meta el) :key index))
                      el)))))
            (render-create-item []
              (when (create-rendered? (:create-new @state))
                (let [{:keys [active-item query]} @state
                      tq (str/trim query)]
                  ((:create-new-item-renderer (props))
                   tq (create-new-item? active-item) #(handle-item-create! tq %)))))
            (default-item-list [lp]
              (let [p       (props)
                    create  ((:render-create-item lp))
                    opts    (cond-> {:no-results (when (nil? create) (:no-results p))}
                              (contains? p :initial-content)
                              (assoc :initial-content (:initial-content p)))
                    content (render-filtered-items lp opts)]
                (when-not (and (nil? content) (nil? create))
                  [menu/menu (merge {:role "listbox"} (:menu-props lp)
                                    {:ref (:items-parent-ref lp)})
                   (when (create-first?) create)
                   content
                   (when-not (create-first?) create)])))
            (active-item-id []
              (let [a (:active-item @state)]
                (cond
                  (nil? a)              nil
                  (create-new-item? a)  (str list-id "-create-item")
                  :else (let [i (active-index)]
                          (when (>= i 0) (str list-id "-item-" i))))))]
      (r/create-class
       {:display-name "blueprint-cljs.query-list"
        :component-did-mount
        (fn [_]
          (when-let [f (:api-ref (props))]
            (f {:set-query!                    set-query!
                :set-active-item!              set-active-item!
                :scroll-active-item-into-view! scroll-active-into-view!})))
        :component-did-update
        (fn [_ [_ prev]]
          (let [p    (props)
                prev (merge defaults prev)]
            (when (and (contains? p :active-item)
                       (not= (:active-item p) (:active-item @state)))
              (reset! check-vp? true)
              (swap! state assoc :active-item (:active-item p)))
            (cond
              (and (some? (:query p)) (not= (:query p) (:query prev)))
              (set-query! (:query p) (:reset-on-query p) p nil)

              (or (not= (:items p) (:items prev))
                  (not (identical? (:item-list-predicate p) (:item-list-predicate prev)))
                  (not (identical? (:item-predicate p) (:item-predicate prev))))
              (set-query! (:query @state)))
            (when @check-vp?
              (reset! check-vp? false)
              (js/requestAnimationFrame scroll-active-into-view!))))
        :reagent-render
        (fn [p]
          (let [p (merge defaults p)
                _ (reset! cur p)
                {:keys [active-item filtered-items query]} @state
                lp {:active-item      active-item
                    :filtered-items   filtered-items
                    :query            query
                    :items            (:items p)
                    :items-parent-ref #(do (reset! parent %) nil)
                    :menu-props       (assoc (:menu-props p) :id list-id)
                    :render-create-item render-create-item
                    :render-item      render-item}]
            ((:renderer p)
             {:active-item         active-item
              :active-item-id      (active-item-id)
              :filtered-items      filtered-items
              :query               query
              :class               (:class p)
              :list-id             list-id
              :handle-item-select  handle-item-select
              :handle-key-down     handle-key-down
              :handle-key-up       handle-key-up
              :handle-paste        handle-paste
              :handle-query-change handle-query-change
              :item-list           ((or (:item-list-renderer p) default-item-list) lp)})))}))))
