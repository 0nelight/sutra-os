(ns sutra-ui.debug
  (:require [goog.object :as gobj]
            [clojure.string :as str]
            [sutra.observe :as obs]
            [sutra-ui.debug.format :as fmt]))

(def ^:private MAX 1000)

(defonce ^:private buffer (atom []))
(defonce ^:private seq-counter (atom 0))
(defonce ^:private drain-cursor (atom 0))
(defonce ^:private installed (atom #{}))

(defonce ^:private sinks (atom []))
(defn add-sink! [f] (swap! sinks conj f) nil)

(declare ring-event!)

(defn record!
  [level src msg & {:keys [stack]}]
  (let [n     (swap! seq-counter inc)
        entry {:n n :t (.now js/Date) :level level :src src :msg msg :stack stack}]
    (swap! buffer (fn [b]
                    (let [b' (conj b entry)
                          c  (count b')]
                      (if (> c MAX) (subvec b' (- c MAX)) b'))))
    (ring-event! entry)
    (doseq [f @sinks] (try (f entry) (catch :default _ nil)))
    entry))

(defn now []
  (if (exists? js/performance) (.now js/performance) (.now js/Date)))

(defn- epoch-now []
  (if (and (exists? js/performance) (.-timeOrigin js/performance))
    (+ (.-timeOrigin js/performance) (.now js/performance))
    (.now js/Date)))

(def ^:private MAX-EVENTS 500)

(defonce ^:private events (atom []))

(defonce ^:private page-id
  (.toString (js/Math.floor (* 1679616 (js/Math.random))) 36))

(defonce ^:private span-counter (atom 0))

(defn next-span! []
  (str "fe-" page-id "-" (swap! span-counter inc)))

(defn record-event!
  [phase ms note span]
  (swap! events
         (fn [v]
           (let [v' (conj v {:kind phase :epoch-ms (- (epoch-now) ms) :dur ms
                             :track "browser" :span span
                             :data (cond-> {} note (assoc :note note))})
                 c  (count v')]
             (if (> c MAX-EVENTS) (subvec v' (- c MAX-EVENTS)) v'))))
  nil)

(defn- ring-payload
  [kind {:keys [level src msg]}]
  (case kind
    :Click {:target msg}
    :State {:atom src :changed msg}
    {:level (name level) :src src :message msg}))

(defn- ring-event!
  [{:keys [level t] :as entry}]
  (when-let [kind (get obs/ring-kinds level)]
    (swap! events
           (fn [v]
             (let [v' (conj v {:kind kind :epoch-ms t :dur 0
                               :track "browser"
                               :data (ring-payload kind entry)})
                   c  (count v')]
               (if (> c MAX-EVENTS) (subvec v' (- c MAX-EVENTS)) v'))))
    nil))

(defn drain-events!
  []
  (let [evs @events]
    (when (seq evs) (reset! events []))
    evs))

(defn measure!
  ([phase t0] (measure! phase t0 nil))
  ([phase t0 note] (measure! phase t0 note nil))
  ([phase t0 note span]
   (let [nm (obs/marker-name phase)
         ms (- (now) t0)]
     (try
       (when (exists? js/performance)
         (.measure js/performance nm #js {:start t0 :duration ms})
         (.clearMeasures js/performance nm))
       (catch :default _ nil))
     (record-event! phase ms note span)
     (record! :perf nm (str (.toFixed ms 1) "ms" (when note (str " · " note))))
     ms)))

(defn mark-ms!
  ([phase ms] (mark-ms! phase ms nil nil))
  ([phase ms note] (mark-ms! phase ms note nil))
  ([phase ms note span] (measure! phase (- (now) ms) note span)))

(defn timed
  ([phase f] (timed phase f nil))
  ([phase f note-fn]
   (let [t0 (now)
         v  (f)]
     (measure! phase t0 (when note-fn (try (note-fn v) (catch :default _ nil))))
     v)))

(defn dump
  ([] (dump 0 nil))
  ([since] (dump since nil))
  ([since level]
   (->> @buffer
        (filter #(> (:n %) since))
        (filter #(or (nil? level) (= level (:level %))))
        vec)))

(defn cursor [] @seq-counter)

(defn clear! []
  (reset! buffer []) (reset! events []) (reset! seq-counter 0) (reset! drain-cursor 0)
  :cleared)

(defn drain-lines
  []
  (let [fresh (dump @drain-cursor)]
    (when (seq fresh) (reset! drain-cursor (:n (last fresh))))
    (apply str (map fmt/line fresh))))

(defn- noise?
  [msg]
  (or (str/starts-with? msg "%cshadow-cljs")
      (str/includes? msg "shadow-cljs: reloading code")
      (str/includes? msg "shadow-cljs: load JS")))

(defn- arg->str
  [x]
  (cond
    (string? x) x
    (nil? x) "nil"
    (or (keyword? x) (symbol? x) (number? x) (boolean? x) (coll? x)) (pr-str x)
    :else (let [s (str x)]
            (if (= s "[object Object]")
              (try (js/JSON.stringify x) (catch :default _ s))
              s))))

(defn- patch-console! [method level]
  (let [k (keyword (str "console-" method))]
    (when-not (contains? @installed k)
      (let [orig (gobj/get js/console method)]
        (gobj/set js/console (str "__sutra_orig_" method) orig)
        (gobj/set js/console method
                  (fn [& args]
                    (let [msg (str/join " " (map arg->str args))]
                      (when-not (noise? msg) (record! level "console" msg)))
                    (.apply orig js/console (to-array args))))
        (swap! installed conj k)))))

(defn- el-label
  [el]
  (when el
    (let [tag (some-> (.-tagName el) (.toLowerCase))
          id  (.-id el)
          cls (let [c (.-className el)] (when (string? c) c))
          txt (some-> (.-textContent el) str str/trim)]
      (str tag
           (when (seq id) (str "#" id))
           (when (seq cls) (str "." (str/replace (str/trim cls) #"\s+" ".")))
           (when (seq txt) (str " «" (subs txt 0 (min 50 (count txt))) "»"))))))

(defn install-click-trace!
  []
  (when-not (contains? @installed :click)
    (.addEventListener js/document "click"
                       (fn [ev] (record! :click "dom"
                                         (str (el-label (.-target ev))
                                              " @" (.-clientX ev) "," (.-clientY ev))))
                       true)
    (swap! installed conj :click)))

(def ^:private FLAECHEN
  (str "[role=dialog],[role=alertdialog],[role=menu],dialog,[aria-modal=true],"
       "[data-trace-sichtbar]"))

(def ^:private SETZEN-MS 300)

(defn- versteckt-durch
  [el]
  (loop [e el]
    (when (and e (not= e (.-documentElement js/document)))
      (let [cs (js/getComputedStyle e)]
        (cond
          (= "none" (.-display cs)) [e "display:none"]
          (= "hidden" (.-visibility cs)) [e "visibility:hidden"]
          (zero? (js/parseFloat (.-opacity cs))) [e "opacity:0"]
          :else (recur (.-parentElement e)))))))

(defn- verdeckt-von
  [el r]
  (let [w (.-innerWidth js/window) h (.-innerHeight js/window)
        x (-> (+ (.-left r) (/ (.-width r) 2)) (max 0) (min (dec w)))
        y (-> (+ (.-top r) (/ (.-height r) 2)) (max 0) (min (dec h)))
        oben (.elementFromPoint js/document x y)]
    (when (and oben (not (.contains el oben))) oben)))

(defn sichtbarkeit
  [el]
  (let [r (.getBoundingClientRect el)
        w (.-innerWidth js/window) h (.-innerHeight js/window)
        warum
        (cond
          (not (.-isConnected el)) "nicht mehr im DOM"
          :else
          (if-let [[e grund] (versteckt-durch el)]
            (str grund (when (not= e el) (str " an " (el-label e))))
            (cond
              (or (zero? (.-width r)) (zero? (.-height r))) "Größe 0"
              (or (>= (.-left r) w) (>= (.-top r) h)
                  (<= (.-right r) 0) (<= (.-bottom r) 0)) "außerhalb des Fensters"
              :else (some->> (verdeckt-von el r) el-label (str "verdeckt von ")))))]
    {:sichtbar? (nil? warum)
     :warum warum
     :box (str (js/Math.round (.-width r)) "×" (js/Math.round (.-height r))
               "@" (js/Math.round (.-left r)) "," (js/Math.round (.-top r))
               " im Fenster " w "×" h)}))

(defn- flaeche-0? [el]
  (let [r (.getBoundingClientRect el)] (or (zero? (.-width r)) (zero? (.-height r)))))

(defn- traeger [el]
  (or (when (flaeche-0? el)
        (some #(when-not (flaeche-0? %) %)
              (array-seq (.querySelectorAll el ".bp-overlay-content,[role=listbox],.bp-menu"))))
      el))

(defn- viewport-text []
  (let [vv (.-visualViewport js/window)
        fk (.-activeElement js/document)
        fr (when (and fk (not= fk (.-body js/document))) (.getBoundingClientRect fk))]
    (str " · vv " (some-> vv .-height js/Math.round) "@" (some-> vv .-offsetTop js/Math.round)
         " scrollY " (js/Math.round (.-scrollY js/window))
         (when fr (str " fokus " (.toLowerCase (.-tagName fk)) " "
                       (js/Math.round (.-width fr)) "×" (js/Math.round (.-height fr))
                       "@" (js/Math.round (.-left fr)) "," (js/Math.round (.-top fr))))
         (when (.hasAttribute (.-documentElement js/document) "data-tastatur") " tastatur"))))

(defn- sichtbarkeit-melden! [el ms]
  (when (.-isConnected el)
    (let [t (traeger el)
          {:keys [sichtbar? warum box]} (sichtbarkeit t)]
      (record! (if sichtbar? :visible :invisible) "dom"
               (str (el-label el) (when (not= t el) (str " → " (el-label t)))
                    (when (> ms SETZEN-MS) (str " nach " ms " ms")) " "
                    (if sichtbar? "sichtbar" (str "UNSICHTBAR: " warum))
                    " · " box (viewport-text))))))

(defonce ^:private geboren (js/WeakMap.))

(defn- flaeche? [el]
  (and (= 1 (.-nodeType el))
       (not (#{"SCRIPT" "STYLE" "LINK" "TEMPLATE" "NOSCRIPT"} (.-tagName el)))
       (or (= (.-parentNode el) (.-body js/document)) (.matches el FLAECHEN))))

(defn- eingehaengt! [node]
  (when (= 1 (.-nodeType node))
    (doseq [el (cons node (array-seq (.querySelectorAll node FLAECHEN)))
            :when (flaeche? el)]
      (.set geboren el (now))
      (doseq [ms [SETZEN-MS 1500 4000]]
        (js/setTimeout #(sichtbarkeit-melden! el ms) ms)))))

(defn- ausgehaengt! [node]
  (when-let [t (.get geboren node)]
    (.delete geboren node)
    (record! :visible "dom" (str (el-label node) " entfernt nach "
                                 (js/Math.round (- (now) t)) " ms"))))

(defn install-visibility-trace!
  []
  (when-not (contains? @installed :visibility)
    (.observe (js/MutationObserver.
               (fn [records]
                 (doseq [rec records]
                   (doseq [n (array-seq (.-addedNodes rec))] (eingehaengt! n))
                   (doseq [n (array-seq (.-removedNodes rec))] (ausgehaengt! n)))))
              (.-body js/document) #js {:childList true :subtree true})
    (swap! installed conj :visibility)))

(defn- changed-keys [old new]
  (->> (distinct (concat (keys old) (keys new)))
       (filterv #(not= (get old %) (get new %)))))

(defn- scalar? [v]
  (or (nil? v) (keyword? v) (number? v) (boolean? v) (string? v)))

(defn- short-val [v]
  (let [s (pr-str v)] (if (> (count s) 40) (str (subs s 0 37) "…") s)))

(defn- changed-summary
  [old new]
  (->> (changed-keys old new)
       (map (fn [k]
              (let [ov (get old k) nv (get new k)]
                (if (and (scalar? ov) (scalar? nv))
                  (str k " " (short-val ov) "→" (short-val nv))
                  (str k)))))
       (str/join ", ")))

(defn install-state-trace!
  [label->atom]
  (doseq [[label a] label->atom
          :let [k (keyword "sutra-debug" (str label))]]
    (when-not (contains? @installed k)
      (add-watch a k
                 (fn [_ _ old new]
                   (when (not= old new)
                     (record! :state (str label)
                              (if (and (map? old) (map? new))
                                (str "[" (changed-summary old new) "]")
                                "changed")))))
      (swap! installed conj k))))

(defn- entry-event!
  [phase entry note]
  (let [start (gobj/get entry "startTime")
        dur   (gobj/get entry "duration")]
    (swap! events
           (fn [v]
             (let [v' (conj v {:kind phase :epoch-ms (+ (.-timeOrigin js/performance) start)
                               :dur dur :track "browser"
                               :data (cond-> {} note (assoc :note note))})
                   c  (count v')]
               (if (> c MAX-EVENTS) (subvec v' (- c MAX-EVENTS)) v'))))))

(defn- server-ms [entry]
  (some->> (gobj/get entry "serverTiming")
           array-seq
           (some #(when (= "app" (gobj/get % "name")) (gobj/get % "duration")))))

(defn- resource-note [entry]
  (let [ms (server-ms entry)]
    (str (.-pathname (js/URL. (gobj/get entry "name")))
         " · ttfb " (js/Math.round (- (gobj/get entry "responseStart")
                                      (gobj/get entry "startTime")))
         "ms · " (gobj/get entry "transferSize") " bytes"
         (when ms (str " · server " ms "ms")))))

(defn- frame-note [entry]
  (some->> (gobj/get entry "scripts")
           array-seq
           (map #(str (gobj/get % "sourceFunctionName") " "
                      (gobj/get % "invoker") " "
                      (js/Math.round (gobj/get % "duration")) "ms"))
           (str/join " | ")))

(defonce ^:private head-seen? (atom false))

(defn- head-visible! [entry]
  (let [at (gobj/get entry "startTime")
        ms (js/Math.round at)
        first? (compare-and-set! head-seen? false true)]
    (swap! events conj {:kind :boot/head-visible
                        :epoch-ms (+ (.-timeOrigin js/performance) (if first? 0 at))
                        :dur (if first? at 0) :track "browser"
                        :data {:note (str ms "ms")}})
    (record! :perf "boot/head-visible" (str ms "ms"))))

(defn- observe! [kind f]
  (when (and (exists? js/PerformanceObserver)
             (some #{kind} (array-seq (.-supportedEntryTypes js/PerformanceObserver))))
    (.observe (js/PerformanceObserver.
               (fn [list _] (doseq [e (array-seq (.getEntries list))] (f e))))
              #js {:type kind :buffered true})
    true))

(defn install-boot-trace!
  []
  (when (and (exists? js/performance) (not (contains? @installed :boot)))
    (swap! installed conj :boot)
    (measure! :boot/main 0)
    (observe! "navigation"
              (fn [e]
                (entry-event! :boot/navigation e
                              (str "ttfb " (js/Math.round (gobj/get e "responseStart"))
                                   "ms · dom "
                                   (js/Math.round (gobj/get e "domContentLoadedEventEnd"))
                                   "ms"))))
    (observe! "resource" #(entry-event! :boot/resource % (resource-note %)))
    (observe! "paint" #(entry-event! :boot/paint % (gobj/get % "name")))
    (observe! "largest-contentful-paint"
              #(entry-event! :boot/paint % "largest-contentful-paint"))
    (or (observe! "long-animation-frame" #(entry-event! :boot/long-task % (frame-note %)))
        (observe! "longtask" #(entry-event! :boot/long-task % nil)))
    (observe! "element"
              #(when (= "object-head" (gobj/get % "identifier")) (head-visible! %)))))

(defn install!
  []
  (patch-console! "log" :console-log)
  (patch-console! "warn" :console-warn)
  (patch-console! "error" :console-error)
  (when-not (contains? @installed :window-errors)
    (set! (.-onerror js/window)
          (fn [msg _src line col err]
            (record! :uncaught (str "window:" line ":" col) (str msg)
                     :stack (some-> err .-stack))
            false))
    (.addEventListener js/window "unhandledrejection"
                       (fn [ev]
                         (let [r (.-reason ev)]
                           (record! :promise-rejection "unhandledrejection"
                                    (str r) :stack (some-> r .-stack)))))
    (swap! installed conj :window-errors))
  (gobj/set js/window "__sutra_debug"
            #js {:dump    (fn [] (clj->js (dump)))
                 :events  (fn [] (clj->js @events))
                 :phases  (clj->js (mapv (comp :name val) obs/phases))
                 :settled (clj->js obs/settled-phases)
                 :visible (fn [sel]
                            (some-> (.querySelector js/document sel) sichtbarkeit clj->js))
                 :clear   (fn [] (clear!))})
  (record! :meta "debug" "instrumentation installed")
  true)

(defn session-header!
  [{:keys [user ontology branch] :as ctx}]
  (record! :meta "session"
           (str "url=" (.. js/window -location -href)
                " build=" (if ^boolean js/goog.DEBUG "dev" "release")
                (when user (str " user=" user))
                (when ontology (str " ontology=" ontology))
                (when branch (str " branch=" branch))))
  ctx)
