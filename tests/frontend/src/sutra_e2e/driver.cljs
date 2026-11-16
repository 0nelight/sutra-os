(ns sutra-e2e.driver
  (:require ["playwright-core" :refer [chromium]]
            ["fs" :as fs]
            ["readline" :as readline]
            [sutra-e2e.object-set :as oset]
            [clojure.string :as str]
            [clojure.set :as set]
            [cljs.reader :as edn]))

(def CDP-PORT 9333)
(def CDP-URL  (str "http://127.0.0.1:" CDP-PORT))
(def APP-URL  "http://localhost:8030/")
(def CHROMIUM "/usr/bin/chromium")
(def TRANSIT  "application/transit+json")

(defn- env [k] (some-> js/process .-env (aget k)))
(defn- backend [] (or (env "SUTRA_HTTP_URL") "http://localhost:8030"))
(defn- shots-dir [] (or (env "SHOTS_DIR") (str (.cwd js/process) "/shots")))

(defn- by-text [page t] (.first (.getByText page t #js {:exact false})))

(defn- do-step [page s]
  (cond
    (contains? s :sleep) (.waitForTimeout page (:sleep s))
    (:goto s)      (.goto page (:goto s) #js {:waitUntil "domcontentloaded"})
    (:reload s)    (.reload page)
    (:wait-text s) (.waitFor (by-text page (:wait-text s)) #js {:timeout 8000})
    (:wait-sel s)  (.waitFor (.first (.locator page (:wait-sel s))) #js {:timeout 8000})
    (:click s)     (.click (by-text page (:click s)) #js {:timeout 8000})
    (:click-sel s) (.click (.first (.locator page (:click-sel s))) #js {:timeout 8000})
    (:fill s)      (.fill (.first (.locator page (first (:fill s)))) (second (:fill s)))
    (:select s)    (.selectOption (.first (.locator page (first (:select s)))) (second (:select s)))
    (:eval s)      (.evaluate page (js/eval (:eval s)))
    (:shot s)      (.screenshot page #js {:path (str (shots-dir) "/" (:shot s) ".png")})
    :else          (js/Promise.resolve)))

(defn- run-steps [page steps]
  (reduce
   (fn [p step]
     (.then p (fn [_]
                (-> (do-step page step)
                    (.then (fn [r]
                             (println "  ok  " (pr-str step)
                                      (when (:eval step) (str "=> " (js/JSON.stringify r))))
                             r))
                    (.catch (fn [e]
                              (println "  STEP-ERR" (pr-str step) "::"
                                       (first (str/split-lines (.-message e))))))))))
   (js/Promise.resolve)
   steps))

(defn- attach-capture [page]
  (let [cap (atom {:console [] :errors [] :net []})]
    (.on page "pageerror" (fn [e] (swap! cap update :errors conj (.-message e))))
    (.on page "console"
         (fn [m] (let [t (.type m)]
                   (when (#{"error" "warning"} t)
                     (swap! cap update :console conj (str "[" t "] " (.text m)))))))
    (.on page "response"
         (fn [r] (let [u (.url r)]
                   (when (str/includes? u "/api/")
                     (swap! cap update :net conj
                            (str (.status r) " " (.method (.request r)) " "
                                 (.-pathname (js/URL. u))))))))
    cap))

(defn- dump-capture [cap {:keys [before after]} target]
  (let [{:keys [console errors net]} @cap
        emptied? (and before (< after 40) (< after (* 0.25 (max before 1))))]
    (println (str "\n── action under test: " (pr-str target) " ──"))
    (println (str "   app innerText length: " before " → " after
                  (when emptied? "   ⚠ COLLAPSED TO (near-)EMPTY PAGE")))
    (println "── render exceptions (pageerror) ──")
    (if (seq errors) (doseq [e errors] (println "  ✗" e)) (println "  (none)"))
    (println "── /api calls during the action (status method path) ──")
    (if (seq net) (doseq [n net] (println "  " n)) (println "  (none fired)"))
    (println "── console errors/warnings ──")
    (if (seq console) (doseq [c console] (println "  " c)) (println "  (none)"))
    (println "\nverdict:")
    (cond
      (seq errors)
      (println "  → render-time exception during the action = FE code bug. Open the .cljs defn in the stack.")
      (and emptied? (empty? net))
      (println
       (str "  → page went BLANK with NO api call = cljs view-dispatch/route bug: the click\n"
            "    switched to a view whose render branch produces nothing. Read that branch."))
      (and emptied? (seq net))
      (println
       (str "  → page went BLANK and api fired = view dispatched + fetched but rendered empty:\n"
            "    check the api call(s) above for non-2xx/empty payload, then the consuming branch."))
      (empty? net)
      (println
       "  → no api call fired = pure cljs view-dispatch. Read the component's render branch.")
      :else
      (println
       "  → api fired, page intact = inspect calls above for non-2xx/empty, then the backend log."))))

(def READY-FN
  (js/eval "() => { const r = document.getElementById('app');
                    return !!r && r.innerText.trim().length > 15 && !/Loading Sutra-OS/.test(r.innerText); }"))

(def APP-LEN-FN
  (js/eval "() => ((document.getElementById('app')||document.body).innerText||'').length"))

(defn- sel-visible? [page sel]
  (-> (.isVisible (.first (.locator page sel)))
      (.catch (fn [_] false))))

(defn- app-len [page]
  (-> (.evaluate page APP-LEN-FN)
      (.catch (fn [_] 0))))

(defn- maybe-skip-login [page]
  (let [loc (.first (.locator page ".login__guest-link"))]
    (-> (.isVisible loc)
        (.then (fn [v]
                 (if v
                   (-> (.click loc #js {:timeout 5000})
                       (.then #(.waitForTimeout page 1200)))
                   (js/Promise.resolve nil))))
        (.catch (fn [_] nil)))))

(defn- autoboot
  ([page] (autoboot page APP-URL))
  ([page url]
  (-> (.goto page url #js {:waitUntil "domcontentloaded"})
      (.then #(.waitForFunction page READY-FN #js {} #js {:timeout 30000}))
      (.then #(maybe-skip-login page))
      (.then #(.waitForFunction page READY-FN #js {} #js {:timeout 30000}))
      (.then #(.waitForTimeout page 400)))))

(def CLICKABLES-FN
  (js/eval
   "() => {
     const vis = el => { const r = el.getBoundingClientRect(); const s = getComputedStyle(el);
       return r.width>2 && r.height>2 && s.visibility!=='hidden' && s.display!=='none' && s.pointerEvents!=='none'; };
     const uniq = el => {
       const parts=[]; let e=el;
       while(e && e.nodeType===1 && e.tagName!=='HTML' && e.tagName!=='BODY'){
         let p=e.tagName.toLowerCase(); const par=e.parentElement;
         if(par){ const sib=[...par.children].filter(c=>c.tagName===e.tagName);
                  if(sib.length>1) p+=':nth-of-type('+(sib.indexOf(e)+1)+')'; }
         parts.unshift(p);
         if(e.id){ try{ if(document.querySelectorAll('#'+CSS.escape(e.id)).length===1){ parts[0]='#'+CSS.escape(e.id); break; } }catch(x){} }
         e=par; }
       return parts.join('>'); };
     const q='button,a[href],[role=button],[role=tab],[role=menuitem],[role=link],[onclick],select,[class*=tile],[class*=card],[class*=__item],[class*=nav],[tabindex]';
     const seen=new Set(); const out=[];
     for(const el of document.querySelectorAll(q)){
       if(!vis(el)) continue;
       const u=uniq(el); if(seen.has(u)) continue; seen.add(u);
       out.push({sel:u, label:(el.innerText||el.getAttribute('aria-label')||el.tagName).trim().replace(/\\s+/g,' ').slice(0,40)});
       if(out.length>=40) break; }
     return out; }"))

(defn- clickables [page]
  (-> (.evaluate page CLICKABLES-FN)
      (.then #(js->clj % :keywordize-keys true))
      (.catch (fn [_] []))))

(defn- replay [page setup path]
  (-> (autoboot page)
      (.then (fn [_] (run-steps page setup)))
      (.then (fn [_]
               (reduce (fn [pr sel]
                         (.then pr (fn [_]
                                     (-> (.click (.first (.locator page sel)) #js {:timeout 6000})
                                         (.then #(.waitForTimeout page 450))
                                         (.catch (fn [_] nil))))))
                       (js/Promise.resolve)
                       path)))))

(defn- tokens [s]
  (->> (str/split (str/lower-case (str s)) #"[^a-z]+")
       (filter #(> (count %) 3)) set))

(defn- rank [cands target-toks]
  (->> cands
       (map-indexed (fn [i c]
                      [(- (count (set/intersection target-toks
                                                   (tokens (str (:sel c) " " (:label c))))))
                       i c]))
       (sort-by (juxt first second))
       (mapv #(nth % 2))))

(defn- find-path
  [page setup target {:keys [max-depth budget breadth] :or {max-depth 3 budget 50 breadth 16}}]
  (let [t-toks (tokens target)
        left   (atom budget)
        seen   (atom #{})]
    (letfn [(visit [queue]
              (if (or (empty? queue) (<= @left 0))
                (js/Promise.resolve nil)
                (let [path (first queue)]
                  (swap! left dec)
                  (-> (replay page setup path)
                      (.then (fn [_] (sel-visible? page target)))
                      (.then
                       (fn [vis]
                         (cond
                           vis (js/Promise.resolve path)
                           (>= (count path) max-depth) (visit (rest queue))
                           :else
                           (-> (clickables page)
                               (.then (fn [cands]
                                        (let [sig [(.url page) (mapv :sel cands)]]
                                          (if (contains? @seen sig)
                                            (visit (rest queue))
                                            (do (swap! seen conj sig)
                                                (let [kids (->> (rank cands t-toks)
                                                                (take breadth)
                                                                (map #(conj path (:sel %))))]
                                                  (visit (concat (rest queue) kids))))))))))))))))]
      (visit [[]]))))

(defn- debug-cmd [steps]
  (let [setup  (vec (butlast steps))
        action (last steps)
        target (or (:click-sel action) (:click action))]
    (-> (.launch chromium #js {:executablePath CHROMIUM :headless true
                               :args #js ["--no-sandbox" "--disable-gpu"]})
        (.then
         (fn [b]
           (-> (.newPage b #js {:viewport #js {:width 1400 :height 900}})
               (.then
                (fn [page]
                  (let [cap (attach-capture page)]
                    (-> (autoboot page)
                        (.then (fn [_]
                                 (println "✓ autobooted (guest login dismissed, app rendered)")
                                 (run-steps page setup)))
                        (.then (fn [_] (sel-visible? page target)))
                        (.then (fn [vis]
                                 (cond
                                   (nil? target) (js/Promise.resolve nil)
                                   vis           (js/Promise.resolve [])
                                   :else
                                     (do
                                       (println
                                        (str "… not on screen — BFS-discovering a clickpath to "
                                             target " (≤50 probes, depth ≤3)"))
                                       (find-path page setup target {})))))
                        (.then (fn [path]
                                 (cond
                                   (nil? target) (js/Promise.resolve :no-target)
                                   (nil? path)
                                   (do
                                     (println (str "✗ no clickpath to " target " from a cold boot —"
                                                   " selector is unreachable by clicking\n"
                                                   "    (likely gated behind seeded data). Read the"
                                                   " cljs render branch next."))
                                     (js/Promise.resolve :unreachable))
                                   (empty? path)
                                   (do (println "✓ target already on screen after boot/setup")
                                       (js/Promise.resolve :here))
                                   :else
                                   (do (println (str "✓ discovered clickpath (" (count path)
                                                     " click(s)): " (pr-str path)))
                                       (-> (replay page setup path) (.then (fn [_] :here)))))))
                        (.then (fn [state]
                                 (if (#{:no-target :unreachable} state)
                                   (js/Promise.resolve :skip)
                                   (-> (app-len page)
                                       (.then (fn [before]
                                                (reset! cap {:console [] :errors [] :net []})
                                                (-> (do-step page action)
                                                    (.then #(.waitForTimeout page 1200))
                                                    (.then #(app-len page))
                                                    (.then (fn [after]
                                                             {:before before :after after}))
                                                    (.catch (fn [e]
                                                              (println "ACTION-ERR ::"
                                                                       (.-message e))
                                                              {:before before :after 0})))))))))
                        (.then (fn [sz]
                                 (when-not (= sz :skip) (dump-capture cap sz target))))
                        (.then #(.close b)))))))))
        (.catch (fn [e] (println "DEBUG-ERR ::" (.-message e)))))))

(defn- handle-line [page base chain line]
  (when-not (str/blank? line)
    (swap! chain
           (fn [p]
             (-> p
                 (.then (fn [_] (oset/read-set page base (edn/read-string line))))
                 (.then (fn [r] (println (str "RESULT " (pr-str r)))))
                 (.catch (fn [e]
                           (println (str "ERROR "
                                         (first
                                          (str/split-lines (str (or (.-message e) e))))))))))))
  nil)

(defn- serve-reads [ctx b page base]
  (println "READY")
  (let [chain (atom (js/Promise.resolve))
        ri    (.createInterface readline #js {:input (.-stdin js/process)})]
    (.on ri "line"  (fn [line] (handle-line page base chain line)))
    (.on ri "close"
         (fn [] (swap! chain (fn [p] (-> p
                                         (.then (fn [_] (.close ctx)))
                                         (.then (fn [_] (.close b))))))))
    nil))

(defn- env-int [k d]
  (let [v (env k)]
    (if (and v (seq v)) (js/parseInt v) d)))

(defn- headful? [] (= "1" (env "SUTRA_E2E_HEADFUL")))

(defn- reads-cmd [url]
  (let [base (if (str/ends-with? url "/") url (str url "/"))
        w    (env-int "SUTRA_E2E_WIDTH" 1600)
        h    (env-int "SUTRA_E2E_HEIGHT" 1000)]
    (-> (.launch chromium #js {:executablePath CHROMIUM
                               :headless (not (headful?))
                               :slowMo   (env-int "SUTRA_E2E_SLOWMO" 0)
                               :args #js ["--no-sandbox" "--disable-gpu"
                                          "--window-position=0,0"
                                          (str "--window-size=" w "," h)]})
        (.then
         (fn [b]
           (let [dir (env "SUTRA_E2E_VIDEO")]
             (-> (.newContext b
                              (cond
                                (and dir (seq dir))
                                #js {:viewport #js {:width w :height h}
                                     :recordVideo #js {:dir dir
                                                       :size #js {:width w :height h}}}
                                (headful?) #js {:viewport nil}
                                :else      #js {:viewport #js {:width w :height h}}))
                 (.then (fn [c]
                          (-> (.addInitScript
                               c
                               "try { localStorage.setItem('sutra.locale', 'en') } catch (e) {}")
                              (.then #(.newPage c))
                              (.then (fn [page]
                                       (-> (autoboot page base)
                                           (.then (fn [_] (serve-reads c b page base)))))))))))))
        (.catch (fn [e]
                  (println (str "ERROR " (.-message e)))
                  (.exit js/process 1))))))

(defn- pick-page [b]
  (let [pages (.pages (aget (.contexts b) 0))]
    (or (first (filter #(str/includes? (.url %) "localhost:8030") pages))
        (aget pages 0))))

(defn- run-cmd [steps]
  (-> (.connectOverCDP chromium CDP-URL)
      (.then (fn [b] (-> (run-steps (pick-page b) steps) (.then #(.close b)))))
      (.catch (fn [e] (println "CONNECT-ERR ::" (.-message e))))))

(defn- serve []
  (-> (.launch chromium
               #js {:executablePath CHROMIUM :headless true
                    :args #js ["--no-sandbox" "--disable-gpu"
                               (str "--remote-debugging-port=" CDP-PORT)]})
      (.then (fn [b] (.newPage b #js {:viewport #js {:width 1400 :height 900}})))
      (.then (fn [page]
               (-> (.goto page APP-URL #js {:waitUntil "domcontentloaded"})
                   (.then #(.waitForFunction page READY-FN #js {} #js {:timeout 30000}))
                   (.then #(println (str "RUNTIME-READY cdp=" CDP-PORT))))))
      (.catch (fn [e] (println "SERVE-ERR ::" (.-message e)))))
  (js/setInterval (fn []) 1e9))

(defn- up? [url]
  (-> (js/fetch url) (.then (fn [_] true)) (.catch (fn [_] false))))

(defn- prepare []
  (.mkdirSync fs (shots-dir) #js {:recursive true})
  (-> (up? (str (backend) "/api/snapshot"))
      (.then (fn [ok]
               (if ok
                 (println (str "✓ prepared — backend healthy at " (backend)))
                 (do (println (str "✗ backend not reachable at " (backend)
                                   " — run ./bin/start.sh from repo root first"))
                     (.exit js/process 1)))))))

(defn- poll-cdp []
  (-> (js/fetch (str CDP-URL "/json"))
      (.then #(.text %))
      (.then #(str/includes? % "localhost:8030"))
      (.catch (fn [_] false))))

(defn- wait-ready [n]
  (-> (poll-cdp)
      (.then (fn [ready?]
               (cond
                 ready?   (println (str "✓ CDP browser up + app page present on " CDP-URL))
                 (<= n
                     0) (do (println (str "✗ " CDP-URL
                                          " not ready — launch 'serve' via run_in_background"))
                              (.exit js/process 1))
                 :else    (js/Promise. (fn [res]
                                         (js/setTimeout #(res (wait-ready (dec n))) 500))))))))

(defn- status []
  (-> (js/Promise.all #js [(up? (str CDP-URL "/json")) (up? (str (backend) "/api/snapshot"))])
      (.then (fn [r]
               (println (str "browser: " (if (aget r 0) "UP" "down") " (" CDP-URL ")"))
               (println (str "backend: " (if (aget r 1) "UP" "down") " (" (backend) ")"))))))

(defn main [& args]
  (let [[cmd payload] args]
    (case cmd
      "prepare" (prepare)
      "serve"   (serve)
      "wait"    (wait-ready 60)
      "status"  (status)
      "run"     (run-cmd (edn/read-string payload))
      "debug"   (debug-cmd (edn/read-string payload))
      "reads"   (reads-cmd payload)
      (println
       "usage: driver.js {prepare|serve|wait|status|run '<edn>'|debug '<edn>'|reads <base-url>}"))))
