(ns sutra-e2e.object-set
  (:require ["fs" :as fs]
            [clojure.string :as str]))

(def op->ui
  {:=        "eq"
   :not=     "ne"
   :contains "contains"
   :>        "gt"
   :>=       "gte"
   :<        "lt"
   :<=       "lte"})

(def fn->stat-column
  {:count "count" :min "min" :max "max" :avg "mean" :sum "sum"})

(defn- env [k] (some-> js/process .-env (aget k)))

(defn- env-ms [k d]
  (let [v (env k)]
    (if (and v (seq v)) (js/parseInt v) d)))

(defn- paced? [] (pos? (env-ms "SUTRA_E2E_SLOWMO" 0)))

(defn filters-of [where]
  (vec (for [[field ops] where
             [op v]      (if (map? ops) ops {:= ops})]
         (if-let [ui (get op->ui op)]
           {:field (if (vector? field)
                     (str (name (nth field 1)) ">" (name (nth field 2)))
                     (name field))
            :op ui :value (str v)}
           (throw (ex-info (str "the Object Workspace has no filter for " (pr-str op)
                                " on " (pr-str field))
                           {:op op :field field}))))))

(def READY-FN
  (js/eval "() => { const r = document.getElementById('app');
                    return !!r && r.innerText.trim().length > 15 && !/Loading Sutra-OS/.test(r.innerText); }"))

(def SCRAPE-FN
  (js/eval
   "() => {
      const body = document.querySelector('.explorer-workspace__body');
      const bar  = document.querySelector('.explorer-objectset__toolbar');
      const cnt  = bar && bar.querySelector('.explorer-objectset__count');
      const num  = k => cnt && cnt.dataset[k] != null ? Number(cnt.dataset[k]) : null;
      const t    = body && body.querySelector('table');
      if (!t) return {missing: true, text: body ? body.innerText.slice(0,200) : null};
      const head = [...t.querySelectorAll('thead th')].map(th => th.getAttribute('title') || th.innerText.trim());
      const rows = [...t.querySelectorAll('tbody tr')]
                     .map(tr => [...tr.querySelectorAll('td')].map(td =>
                       (td.querySelector('.sutra-objlink__title') || td).innerText.trim()));
      const keys = [...t.querySelectorAll('tbody tr')].map(tr => tr.dataset.prop || null);
      return {head, rows, keys, counts: {count: num('count'), loaded: num('loaded')}};
    }"))

(def TYPE-COUNT-FN
  (js/eval
   "(t) => {
      const name = [...document.querySelectorAll('.explorer-sidebar__type-name')]
                     .find(e => e.dataset.type === t);
      const row  = name && name.closest('.explorer-sidebar__type-row');
      const c    = row && row.querySelector('.explorer-sidebar__type-count');
      return c && /^\\d+$/.test(c.innerText.trim()) ? Number(c.innerText.trim()) : null;
    }"))

(def BARS-FN
  (js/eval
   "() => [...document.querySelectorAll('.explorer-objectset__bar-row')].map(r => ({
      label: r.querySelector('.explorer-objectset__bar-label').innerText.trim(),
      count: r.querySelector('.explorer-objectset__bar-count').innerText.trim()}))"))

(defn- settle [page tries prev]
  (-> (.evaluate page SCRAPE-FN)
      (.then (fn [r]
               (let [now (js/JSON.stringify r)]
                 (cond
                   (= now prev) (js/Promise.resolve (js->clj r :keywordize-keys true))
                   (<= tries 0) (js/Promise.resolve (js->clj r :keywordize-keys true))
                   :else        (-> (.waitForTimeout page 250)
                                    (.then #(settle page (dec tries) now)))))))))

(defn- pick-type [page type-name]
  (let [sel (str ".explorer-sidebar__type-name[data-type=\"" type-name "\"]")
        loc (.first (.locator page sel))]
    (-> (.waitFor loc #js {:timeout 15000 :state "visible"})
        (.catch (fn [_]
                  (throw (ex-info (str "no object type " type-name
                                       " in the Object Workspace sidebar")
                                  {:type type-name}))))
        (.then #(.click loc #js {:timeout 8000}))
        (.then #(.waitForFunction
                 page
                 (js/eval
                  "() => !!document.querySelector('.explorer-objectset__count[data-count]')")
                 #js {} #js {:timeout 15000})))))

(defn- apply-filter [page {:keys [field op value]}]
  (-> (.click (.first (.locator page "button:has-text(\"+ Filter\")")) #js {:timeout 8000})
      (.then #(.waitForTimeout page 150))
      (.then (fn [_]
               (let [row   (.last (.locator page ".explorer-objectset__filter-row"))
                     input (.first (.locator row "input"))]
                 (-> (.selectOption (.first (.locator row "select")) field)
                     (.then #(.waitForTimeout page 150))
                     (.then #(.selectOption (.nth (.locator row "select") 1) op))
                     (.then #(.waitForTimeout page 150))
                     (.then #(if (paced?)
                               (.pressSequentially input value #js {:delay 110})
                               (.fill input value)))))))
      (.then #(.waitForTimeout page 250))))

(defn- apply-search [page q]
  (let [input (.first (.locator page ".explorer-objectset__filterbar-top input"))]
    (-> (if (paced?)
          (.pressSequentially input q #js {:delay 110})
          (.fill input q))
        (.then #(.waitForTimeout page 250)))))

(defn- header-index [page attr]
  (-> (.evaluate page
                 (js/eval "() => [...document.querySelectorAll('.explorer-workspace__body thead th')].map(t => t.getAttribute('title') || t.innerText.trim())"))
      (.then (fn [hs]
               (let [names (mapv #(first (str/split % #" ")) (js->clj hs))
                     idx   (.indexOf (clj->js names) (name attr))]
                 (if (neg? idx)
                   (throw (ex-info (str "no column " (name attr) " in the rendered table")
                                   {:attr attr :columns names}))
                   idx))))))

(defn- apply-sort [page [attr dir]]
  (-> (header-index page attr)
      (.then (fn [idx]
               (let [th (.nth (.locator page ".explorer-workspace__body thead th") idx)]
                 (-> (.click th #js {:timeout 8000})
                     (.then #(.waitForTimeout page 400))
                     (.then #(when (= :desc dir)
                               (-> (.click th #js {:timeout 8000})
                                   (.then (fn [_] (.waitForTimeout page 400))))))))))))

(defn- show-charts [page]
  (-> (.click (.first (.locator page "button:has-text(\"Charts\")")) #js {:timeout 8000})
      (.then #(.waitForTimeout page 400))))

(defn- show-table [page]
  (-> (.click (.first (.locator page ".explorer-objectset__viz-btn:has-text(\"Table\")"))
              #js {:timeout 8000})
      (.then #(.waitForTimeout page 400))))

(defn- menu-item
  [page button label]
  (-> (.click (.first (.locator page button)) #js {:timeout 8000})
      (.then #(.waitForTimeout page 200))
      (.then #(.click (.first (.locator page
                                        (str ".explorer-action__dropdown-item:has-text(\"" label
                                             "\")")))
                      #js {:timeout 8000}))))

(def ^:private more-menu ".explorer-objectset__toolbar button[title=\"More actions\"]")

(defn- show-all-columns [page]
  (-> (.click (.first (.locator page
                                ".explorer-objectset__table-bar .explorer-action__menu > button"))
              #js {:timeout 8000})
      (.then #(.click (.first (.locator page ".explorer-objectset__columns-bar button"))
                      #js {:timeout 8000}))
      (.then
       #(.click (.first (.locator page
                                  ".explorer-objectset__table-bar .explorer-action__backdrop"))
                #js {:timeout 8000}))
      (.then #(.waitForTimeout page 200))))

(defn- breakdown-by [page attr]
  (let [card (.first (.locator page ".explorer-objectset__chart-card"))]
    (-> (.selectOption (.first (.locator card "select")) (name attr))
        (.then #(.waitForTimeout page 400)))))

(defn- prepare [page base {:keys [find where search order-by]}]
  (let [fs* (filters-of where)]
    (-> (.goto page (str base "object-workspace") #js {:waitUntil "domcontentloaded"})
        (.then #(.waitForFunction page READY-FN #js {} #js {:timeout 30000}))
        (.then #(pick-type page (name find)))
        (.then #(show-all-columns page))
        (.then (fn [_]
                 (reduce (fn [p f] (.then p (fn [_] (apply-filter page f))))
                         (js/Promise.resolve) fs*)))
        (.then (fn [_] (if (str/blank? (str search))
                         (js/Promise.resolve nil)
                         (apply-search page search))))
        (.then (fn [_] (if order-by
                         (apply-sort page order-by)
                         (js/Promise.resolve nil)))))))

(defn- table-of [page {:keys [find]}]
  (-> (js/Promise.all #js [(settle page 8 nil) (.evaluate page TYPE-COUNT-FN (name find))])
      (.then (fn [[{:keys [head rows counts missing text]} total]]
               (if missing
                 (throw (ex-info (str "the object set of " (name find)
                                      " rendered no table: " text)
                                 {:find find}))
                 (let [cols (mapv #(keyword (first (str/split % #" "))) (rest head))]
                   (merge {:object-type find
                           :columns     cols
                           :rows        (mapv (fn [r] (zipmap cols (rest r))) rows)
                           :total       total}
                          counts)))))))

(defn- stats-of [page]
  (-> (settle page 8 nil)
      (.then (fn [{:keys [head rows keys]}]
               (let [cols (mapv #(str/lower-case (str/trim %)) head)]
                 (into {} (for [[r k] (map vector rows (concat keys (repeat nil)))]
                            [(keyword (or k (first r)))
                             (zipmap (rest cols) (rest r))])))))))

(defn- metrics-of [stats aggs]
  (into {} (for [{:keys [fn attr name] :as a} aggs
                 :let [col (get fn->stat-column fn)
                       k   (or name (keyword (str (clojure.core/name fn))))]]
             (if-not col
               (throw (ex-info (str "the Statistics card has no " (pr-str fn) " column")
                               {:agg a}))
               [k (if (= :count fn)
                    (get-in stats [(or attr (ffirst stats)) "count"])
                    (get-in stats [attr col]))]))))

(defn- read-stats [page {:keys [find aggs]}]
  (-> (show-charts page)
      (.then (fn [_] (stats-of page)))
      (.then (fn [stats]
               {:object-type  find
                :aggregations [{:group {} :metrics (metrics-of stats aggs)}]}))))

(defn- read-breakdown [page {:keys [find group-by aggs]}]
  (let [attr (first group-by)]
    (when (< 1 (count group-by))
      (throw (ex-info "the Breakdown card groups by ONE property, not several"
                      {:group-by group-by})))
    (when-not (every? #(= :count (:fn %)) aggs)
      (throw (ex-info (str "the Breakdown card counts; " (pr-str (mapv :fn aggs))
                           " needs the Statistics card on a filtered subset")
                      {:aggs aggs})))
    (-> (show-charts page)
        (.then (fn [_] (breakdown-by page attr)))
        (.then (fn [_] (.evaluate page BARS-FN)))
        (.then (fn [bars]
                 (let [k (or (:name (first aggs)) :count)]
                   {:object-type  find
                    :aggregations (mapv (fn [{:keys [label count]}]
                                          {:group   {attr label}
                                           :metrics {k count}})
                                        (js->clj bars :keywordize-keys true))}))))))

(defn- pick-bucket [page {:keys [pick]}]
  (let [[attr value] (first pick)]
    (-> (show-charts page)
        (.then (fn [_] (breakdown-by page attr)))
        (.then
         (fn [_]
           (.click
            (.first
             (.locator page
                       (str
                        ".explorer-objectset__bar-row:has(.explorer-objectset__bar-label:text-is(\""
                        value "\"))")))
            #js {:timeout 8000})))
        (.then #(.waitForTimeout page 400)))))

(def ^:private link-sections ".capp-l-linkside")

(def RELATIONS-FN
  (js/eval
   (str "() => {
      const seen = new Set(), out = [];
      for (const g of document.querySelectorAll('" link-sections "')) {
        const edge = [...g.classList].find(c => c.startsWith('capp-l-linkside--')).slice(17);
        for (const tr of g.querySelectorAll('tbody tr')) {
          const peer = ((tr.querySelector('.sutra-objlink__title') || {}).innerText || '').trim();
          const k = edge + '|' + peer;
          if (peer && !seen.has(k)) { seen.add(k); out.push([edge, null, peer]); }
        }
      }
      return out;
    }")))

(defn- read-relations [page {:keys [find]}]
  (-> (.click
       (.first
        (.locator page
                  ".explorer-workspace__body tbody tr .explorer-objectset__title-cell .sutra-objlink"))
       #js {:timeout 8000})
      (.then #(.waitFor (.first (.locator page (str link-sections " tbody tr")))
                        #js {:timeout 15000}))
      (.then #(.waitForTimeout page 600))
      (.then (fn [_] (.evaluate page RELATIONS-FN)))
      (.then (fn [rows]
               {:object-type find
                :relations   (mapv (fn [r]
                                     {:edge       (nth r 0 nil)
                                      :peer-type  (nth r 1 nil)
                                      :peer       (nth r 2 nil)})
                                   (js->clj rows))}))))

(def PALETTE-FN
  (js/eval
   "() => [...document.querySelectorAll('.palette__row')].map(r => ({
      kind:  ((r.querySelector('.palette__kind')||{}).innerText || '').split('·').pop(),
      label: (r.querySelector('.sutra-objlink__title, .dataconn-importrow__name')||{}).innerText || '',
      sub:   (r.querySelector('.palette__sub')||{}).innerText || ''}))"))

(defn- global-search [page base q]
  (-> (.goto page (str base "object-workspace") #js {:waitUntil "domcontentloaded"})
      (.then #(.waitForFunction page READY-FN #js {} #js {:timeout 30000}))
      (.then #(.press (.-keyboard page) "Control+k"))
      (.then #(.waitFor (.first (.locator page ".palette__input"))
                        #js {:timeout 8000 :state "visible"}))
      (.then (fn [_]
               (let [inp (.first (.locator page ".palette__input"))]
                 (if (paced?)
                   (.pressSequentially inp q #js {:delay 110})
                   (.fill inp q)))))
      (.then #(.waitForTimeout page 1500))
      (.then (fn [_] (.evaluate page PALETTE-FN)))
      (.then (fn [rows]
               {:query q
                :hits  (mapv (fn [h] (update-vals h str/trim))
                             (js->clj rows :keywordize-keys true))}))))

(defn- pivot [page link-type]
  (let [btn (.first (.locator page (str ".explorer-objectset__linked button[data-side=\""
                                        (name link-type) "\"]")))]
    (-> (.count btn)
        (.then (fn [n]
                 (if (zero? n)
                   (throw (ex-info (str "no Linked-objects pivot for " (name link-type)
                                        " on this set")
                                   {:link link-type}))
                   (.click btn #js {:timeout 8000}))))
        (.then #(.waitFor (.first (.locator page
                                            ".explorer-objectset__pivot-chip:not([data-pending])"))
                          #js {:timeout 15000}))
        (.then #(.waitForTimeout page 400)))))

(defn- do-login [page base {:keys [username password]}]
  (-> (.goto page base #js {:waitUntil "domcontentloaded"})
      (.then #(.waitForFunction page READY-FN #js {} #js {:timeout 30000}))
      (.then #(.waitFor (.first (.locator page ".login__input"))
                        #js {:timeout 10000 :state "visible"}))
      (.then #(.fill (.nth (.locator page ".login__input") 0) username))
      (.then #(.fill (.nth (.locator page ".login__input") 1) password))
      (.then #(.click (.first (.locator page ".login__submit")) #js {:timeout 8000}))
      (.then #(-> (.waitFor (.first (.locator page ".login__card"))
                            #js {:timeout 15000 :state "detached"})
                  (.catch (fn [_]
                            (-> (.innerText (.first (.locator page ".login__card")))
                                (.then (fn [m]
                                         (throw (ex-info (str "still on the sign-in form: "
                                                              (str/replace (str m) #"\s+" " "))
                                                         {})))))))))
      (.then #(.waitForTimeout page 1500))
      (.then (fn [_] {:logged-in username}))))

(def SAVED-FN
  (js/eval
   "() => ({flash: (f => f ? (f.innerText + ' ' + (f.dataset.rid || '')).trim() : null)(
                     document.querySelector('.explorer-objectset__saved-flash')),
            error: (document.querySelector('.explorer-objectset__saved-error')||{}).innerText || null,
            sets:  [...document.querySelectorAll('.explorer-sidebar__saved-row')].map(r => ({
                      label: r.innerText.trim(), rid: r.getAttribute('title')}))})"))

(defn- save-set [page {:keys [find save]}]
  (let [label (if (= :list save) "Save as list" "Save as exploration")
        item  (.first (.locator page
                                (str ".explorer-action__dropdown-item:has-text(\"" label "\")")))]
    (-> (.click (.first (.locator page ".explorer-objectset__save .explorer-action__menu > button"))
                #js {:timeout 8000})
        (.then #(.getAttribute item "class" #js {:timeout 8000}))
        (.then (fn [cls]
                 (when (str/includes? (str cls) "--disabled")
                   (-> (.getAttribute item "title")
                       (.then (fn [why]
                                (throw (ex-info (str "save refused: " why) {}))))))))
        (.then (fn [_] (.click item #js {:timeout 8000})))
        (.then #(.waitForTimeout page 2500))
        (.then (fn [_] (.evaluate page SAVED-FN)))
        (.then (fn [r]
                 (let [{:keys [flash error sets]} (js->clj r :keywordize-keys true)]
                   (if error
                     (throw (ex-info (str "save refused: " error) {}))
                     {:object-type find :saved flash :saved-sets sets})))))))

(defn- open-saved [page {:keys [open-saved]}]
  (-> (.click (.first (.locator page
                                (str ".explorer-sidebar__saved-row[data-kind=\"" open-saved "\"], "
                                     ".explorer-sidebar__saved-row:has-text(\"" open-saved "\")")))
              #js {:timeout 8000})
      (.then #(.waitForTimeout page 2000))))

(def COMPARE-FN
  (js/eval
   "() => {
      const el = document.querySelector('.explorer-objectset__compare');
      if (!el) return null;
      const num = c => { const n = el.querySelector(c); if (!n) return null;
                         const m = n.innerText.match(/(\\d+)\\)?\\s*$/); return m ? parseInt(m[1]) : null; };
      return {a: num('.explorer-objectset__compare-a'), b: num('.explorer-objectset__compare-b'),
              both: num('.explorer-objectset__compare-both'),
              'only-a': num('.explorer-objectset__compare-onlya'),
              'only-b': num('.explorer-objectset__compare-onlyb'),
              text: el.innerText.replace(/\\s+/g,' ').trim()};
    }"))

(defn- clear-filters [page]
  (let [btn (.first (.locator page "button:has-text(\"Remove all filters\")"))]
    (-> (.count btn)
        (.then (fn [n] (if (pos? n)
                         (-> (.click btn #js {:timeout 8000})
                             (.then (fn [_] (.waitForTimeout page 300))))
                         (js/Promise.resolve nil)))))))

(defn- compare-with [page spec]
  (-> (menu-item page more-menu "Compare")
      (.then #(.waitForTimeout page 400))
      (.then (fn [_] (clear-filters page)))
      (.then (fn [_]
               (reduce (fn [p f] (.then p (fn [_] (apply-filter page f))))
                       (js/Promise.resolve) (filters-of (:compare-where spec)))))
      (.then #(.waitForTimeout page 600))
      (.then (fn [_] (.evaluate page COMPARE-FN)))
      (.then (fn [r]
               (if (nil? r)
                 (throw (ex-info "no comparison bar rendered" {}))
                 {:object-type (:find spec)
                  :compare     (js->clj r :keywordize-keys true)})))))

(defn- select-row [page]
  (-> (.click (.first (.locator page ".explorer-workspace__body tbody tr td input[type=checkbox]"))
              #js {:timeout 8000})
      (.then #(.waitForTimeout page 300))))

(defn- fill-param [page [param value]]
  (let [field (.first (.locator page (str ".capp-actionform__row:has("
                                          ".capp-actionform__label:text-matches(\"^"
                                          (name param) "[*]?$\"))")))
        sel   (.first (.locator field "select"))]
    (-> (.count field)
        (.then (fn [n]
                 (when (zero? n)
                   (-> (.allInnerTexts (.locator page ".capp-actionform__label"))
                       (.then (fn [labels]
                                (throw (ex-info (str "the action form has no row for "
                                                     (name param) " — it shows: "
                                                     (str/join " · " (js->clj labels)))
                                                {:param param}))))))))
        (.then (fn [_] (.count sel)))
        (.then (fn [n]
                 (if (pos? n)
                   (.selectOption sel (str value))
                   (let [input (.first (.locator field "input"))]
                     (if (paced?)
                       (.pressSequentially input (str value) #js {:delay 110})
                       (.fill input (str value)))))))
        (.then #(.waitForTimeout page 200)))))

(def PREVIEW-FN
  (js/eval
   "() => {
      const p = document.querySelector('.explorer-action__preview');
      const edn = [...p.querySelectorAll('[data-prop]')]
                    .map(li => ':' + li.dataset.prop + ' ' + li.dataset.value).join(' ');
      return p.innerText + '\\n{' + edn + '}';
    }"))

(defn- run-action [page {:keys [find action]}]
  (let [{:keys [name params select preview]} action]
    (-> (if select (select-row page) (js/Promise.resolve nil))
        (.then #(.click (.first (.locator page "button:has-text(\"Actions\")"))
                        #js {:timeout 8000}))
        (.then #(.waitForTimeout page 300))
        (.then #(let [item (.first (.locator page (str ".explorer-action__dropdown-item[title=\""
                                                       (clojure.core/name name) "\"]")))]
                  (-> (.count item)
                      (.then (fn [n]
                               (if (zero? n)
                                 (throw (ex-info (str "the set-level Actions menu does not list "
                                                      (clojure.core/name name))
                                                 {:action name}))
                                 (.click item #js {:timeout 8000})))))))
        (.then #(.waitFor (.first (.locator page ".capp-actionform"))
                          #js {:timeout 8000 :state "visible"}))
        (.then (fn [_]
                 (reduce (fn [p kv] (.then p (fn [_] (fill-param page kv))))
                         (js/Promise.resolve) (seq params))))
        (.then (fn [_]
                 (if preview
                   (-> (.click (.first (.locator page "button:text-is(\"Preview\")"))
                               #js {:timeout 8000})
                       (.then #(.waitFor (.first (.locator page ".explorer-action__preview"))
                                         #js {:timeout 10000 :state "visible"}))
                       (.then #(.evaluate page PREVIEW-FN)))
                   (js/Promise.resolve nil))))
        (.then (fn [prev]
                 (-> (.click (.first (.locator page "button:text-is(\"Run\")")) #js {:timeout 8000})
                     (.then #(.waitFor (.first (.locator page ".capp-actionform"))
                                       #js {:timeout 20000 :state "detached"}))
                     (.then #(.waitForTimeout page 800))
                     (.then (fn [_] {:object-type find
                                     :applied     name
                                     :preview     prev})))))
        (.catch (fn [e]
                  (-> (.innerText (.first (.locator page ".explorer-action__error")))
                      (.then (fn [msg] (throw (ex-info (str "action " (clojure.core/name name)
                                                            " failed: " msg) {}))))
                      (.catch (fn [_] (throw e)))))))))

(defn- parse-csv [text]
  (let [[head & body] (remove str/blank? (str/split-lines text))
        cols (mapv keyword (str/split head #","))]
    {:columns cols
     :rows    (mapv (fn [l] (zipmap cols (str/split l #"," -1))) body)}))

(defn- read-export [page {:keys [find export]}]
  (let [json? (= :json export)
        label (if json? "Export as JSON" "Export as CSV")]
    (-> (js/Promise.all
         #js [(.waitForEvent page "download" #js {:timeout 20000})
              (menu-item page more-menu label)])
        (.then (fn [r] (.path (aget r 0))))
        (.then (fn [p] (.readFileSync fs p "utf8")))
        (.then (fn [text]
                 (if json?
                   (let [rows (mapv #(dissoc % :id)
                                    (js->clj (js/JSON.parse text) :keywordize-keys true))]
                     {:object-type find
                      :columns     (vec (keys (first rows)))
                      :rows        rows})
                   (merge {:object-type find} (parse-csv text))))))))

(defn- read-in-set [page base spec]
  (-> (prepare page base spec)
      (.then (fn [_]
               (cond
                 (and (:pivot spec) (:save spec))
                 (-> (pivot page (:pivot spec))
                     (.then (fn [_] (save-set page (assoc spec :find (:pivot-to spec))))))
                 (:save spec)      (save-set page spec)
                 (:compare-where spec) (compare-with page spec)
                 (:pivot spec)     (-> (pivot page (:pivot spec))
                                       (.then (fn [_] (table-of page {:find (:pivot-to spec)}))))
                 (:action spec)    (run-action page spec)
                 (:export spec)    (read-export page spec)
                 (:relations spec) (read-relations page spec)
                 (:pick spec)     (-> (pick-bucket page spec)
                                      (.then (fn [_] (show-table page)))
                                      (.then (fn [_] (table-of page spec))))
                 (:group-by spec) (read-breakdown page spec)
                 (:aggs spec)     (read-stats page spec)
                 :else            (table-of page spec))))
      (.then (fn [r]
               (-> (.waitForTimeout page (env-ms "SUTRA_E2E_DWELL" 0))
                   (.then (fn [_]
                            (if-let [shot (:shot spec)]
                              (-> (.screenshot page #js {:path shot})
                                  (.then (fn [_] (assoc r :shot shot))))
                              r))))))))

(defn- read-set* [page base spec]
  (if-let [q (:search-objects spec)]
    (-> (global-search page base q)
        (.then (fn [r] (-> (.waitForTimeout page (env-ms "SUTRA_E2E_DWELL" 0))
                           (.then (fn [_] r))))))
    (read-in-set page base spec)))

(defn read-set [page base spec]
  (cond
    (:login spec)
    (-> (do-login page base (:login spec))
        (.then (fn [r] (-> (.waitForTimeout page (env-ms "SUTRA_E2E_DWELL" 0))
                           (.then (fn [_] r))))))

    (:open-saved spec)
    (-> (.goto page (str base "object-workspace") #js {:waitUntil "domcontentloaded"})
        (.then #(.waitForFunction page READY-FN #js {} #js {:timeout 30000}))
        (.then #(.waitForTimeout page 1500))
        (.then (fn [_] (open-saved page spec)))
        (.then (fn [_] (table-of page spec))))

    :else
    (read-set* page base spec)))

