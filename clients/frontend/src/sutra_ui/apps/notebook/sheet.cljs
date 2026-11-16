(ns sutra-ui.apps.notebook.sheet
  (:require [sutra.capps.spreadsheet :as ss]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.sheet :as sheet]
            ["yjs" :as Y]
            [sutra-ui.ui.core :as ui :refer [link divider]]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.api :as api]
            [sutra-ui.apps.notebook.events]
            [sutra-ui.apps.notebook.live :as live]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(def ^:private locale "de")
(def ^:private calc-ms 120)
(def ^:private drop-note "Seine Zellen gehen mit; eine Version hält sie.")

(defn- cells-of [sel] (ss/range-cells (ss/selection-range sel)))

(defn- ymap [^js ydoc k] (.getMap ydoc k))
(defn- rd [s] (when (string? s) (reader/read-string s)))
(defn- ckey [id a] (str id "!" a))

(defn- split-key [k]
  (let [i (str/index-of k "!")] [(subs k 0 i) (subs k (inc i))]))

(defn- y-model
  [ydoc]
  (let [sheets (atom []) cells (atom {}) styles (atom {})]
    (.forEach (ymap ydoc "sheets")
              (fn [v k] (swap! sheets conj (assoc (rd v) :id k))))
    (.forEach (ymap ydoc "cells") (fn [v k] (swap! cells assoc (split-key k) v)))
    (.forEach (ymap ydoc "styles") (fn [v k] (swap! styles assoc (split-key k) (rd v))))
    {:sheets (->> @sheets (sort-by (juxt :pos :id))
                  (reduce (fn [acc s] (if (some #(= (str/lower-case (:name %))
                                                    (str/lower-case (:name s))) acc)
                                        acc (conj acc s)))
                          []))
     :cells  @cells
     :styles @styles}))

(defn- ->str-map [m] (into {} (map (fn [[k v]] [(name k) v])) m))

(defn- view->model
  [sheets]
  {:sheets (vec (map-indexed (fn [i s] {:id (:name s) :name (:name s) :pos i}) sheets))
   :cells  (into {} (for [s sheets [a cv] (->str-map (:cells s)) :when (:input cv)]
                      [[(:name s) a] (:input cv)]))
   :styles (into {} (for [s sheets [a st] (->str-map (:styles s))] [[(:name s) a] st]))})

(defn- request [{:keys [sheets cells styles]}]
  (vec (for [{:keys [id name]} sheets]
         {:name   name
          :cells  (into {} (for [[[i a] x] cells :when (= i id)] [a x]))
          :styles (into {} (for [[[i a] st] styles :when (and (= i id) (seq st))] [a st]))})))

(defn- view-by-name [sheets]
  (into {} (for [s sheets]
             [(:name s) {:cells (->str-map (:cells s)) :styles (->str-map (:styles s))}])))

(defn- seed!
  [^js ydoc base]
  (.transact ydoc
             (fn []
               (doseq [{:keys [id name pos]} (:sheets base)
                       :let [yid (subs (str (random-uuid)) 0 8)]]
                 (.set (ymap ydoc "sheets") yid (pr-str {:name name :pos pos}))
                 (doseq [[[i a] x] (:cells base) :when (= i id)]
                   (.set (ymap ydoc "cells") (ckey yid a) x))
                 (doseq [[[i a] st] (:styles base) :when (= i id)]
                   (.set (ymap ydoc "styles") (ckey yid a) (pr-str st)))))
             "seed"))

(defn- set-input! [ydoc id a x]
  (if (str/blank? x)
    (.delete (ymap ydoc "cells") (ckey id a))
    (.set (ymap ydoc "cells") (ckey id a) x)))

(defn- set-style! [ydoc id a st]
  (let [st (into {} (remove (comp nil? val)) st)]
    (if (empty? st)
      (.delete (ymap ydoc "styles") (ckey id a))
      (.set (ymap ydoc "styles") (ckey id a) (pr-str st)))))

(defn- decimals [n] (if (pos? n) (str "," (apply str (repeat n "0"))) ""))

(defn number-format
  [{:keys [category places thousands? code]}]
  (let [n (or places 2)]
    (case category
      :general  nil
      :number   (str (if thousands? "#.##0" "0") (decimals n))
      :currency (str "#.##0" (decimals n) " €")
      :percent  (str "0" (decimals n) "%")
      :date     (or code "TT.MM.JJJJ")
      :time     (or code "hh:mm")
      :datetime "TT.MM.JJJJ hh:mm"
      :text     "@"
      :custom   (not-empty code))))

(defn format-choice
  [f]
  (let [places (some-> (re-find #",(0+)" (str f)) second count)]
    (cond
      (str/blank? f)                         {:category :general}
      (= "@" f)                              {:category :text}
      (= "TT.MM.JJJJ hh:mm" f)               {:category :datetime}
      (re-matches #"[#0.]+(,0+)? €" f)       {:category :currency :places (or places 0)}
      (re-matches #"0(,0+)?%" f)             {:category :percent :places (or places 0)}
      (re-matches #"(#\.##)?0(,0+)?" f)      {:category :number :places (or places 0)
                                              :thousands? (str/starts-with? f "#")}
      (#{"TT.MM.JJJJ" "TT.MM.JJ" "T. MMMM JJJJ"} f) {:category :date :code f}
      (#{"hh:mm" "hh:mm:ss"} f)              {:category :time :code f}
      :else                                  {:category :custom :code f})))

(def ^:private categories
  [[:general "Standard"] [:number "Zahl"] [:currency "Währung"] [:percent "Prozent"]
   [:date "Datum"] [:time "Uhrzeit"] [:datetime "Datum und Uhrzeit"] [:text "Text"]
   [:custom "Benutzerdefiniert"]])

(defn- cell-style [st cv]
  (cond-> {}
    (:bold st)      (assoc :font-weight "bold")
    (:italic st)    (assoc :font-style "italic")
    (:underline st) (assoc :text-decoration "underline")
    (:align st)     (assoc :text-align (:align st))
    (and (nil? (:align st)) (#{"number" "bool"} (:kind cv))) (assoc :text-align "right")
    (= "error" (:kind cv)) (assoc :color "var(--c-danger-strong)")
    (:color cv)     (assoc :color (:color cv))
    (:color st)     (assoc :color (:color st))
    (:fill st)      (assoc :background (:fill st))
    (:wrap st)      (assoc :white-space "pre-wrap")))

(defn- lock-title [{:keys [by at]}]
  (str "Gesperrt von " (or by "?")
       (when at (str " am " (.toLocaleString (js/Date. at) "de-AT")))
       " — erst entsperren, dann bearbeiten"))

(defn- color-field [label v on]
  [:label {:class "np-sheet-fmt__color"}
   [:span label]
   [:input {:type "color" :value (or v "#111418") :on-change #(on (.. % -target -value))}]
   [link {:on-click #(on nil)} "keine"]])

(defn- format-dialog
  [st apply!]
  (let [f #(:format @st)]
    (fn [_ _]
      (let [{:keys [number] :as x} (f)
            code (number-format number)]
        [overlay/modal {:on-close #(swap! st dissoc :format)
                        :close-on-backdrop? false
                        :class "np-sheet-fmt"
                        :size 520
                        :title (str "Zellen formatieren · " (:range x))
                        :foot [:<>
                               [ui/button {:on-click #(swap! st dissoc :format)}
                                "Abbrechen"]
                               [ui/button {:intent :primary
                                           :on-click #(do (apply! (f)) (swap! st dissoc :format))}
                                "Übernehmen"]]
                        :foot-end? true}
         [:<>
          [:div {:class "np-sheet-fmt__grid"}
           [:section
            [:div {:class "np-section-title"} "Zahl"]
            [ui/html-select {:fill true :value (name (:category number))
                             :on-change #(swap! st update :format assoc-in [:number :category]
                                                (keyword (.. % -target -value)))}
             (for [[k l] categories] ^{:key k} [:option {:value (name k)} l])]
            (when (#{:number :currency :percent} (:category number))
              [:label {:class "np-row"} "Dezimalstellen"
               [ui/numeric-input
                {:min 0 :max 10 :minor-step-size nil :major-step-size nil :small true
                 :value (or (:places number) 2) :style {:width "5em"}
                 :on-value-change (fn [_ s _]
                                    (let [n (js/parseInt s)]
                                      (swap! st update :format assoc-in [:number :places]
                                             (if (js/isNaN n) 0 (max 0 (min 10 n))))))}]])
            (when (= :number (:category number))
              [ui/checkbox
               {:class "np-check" :label "Tausendertrennzeichen (.)"
                :checked (boolean (:thousands? number))
                :on-change #(swap! st update :format assoc-in [:number :thousands?]
                                   (.. % -target -checked))}])
            (case (:category number)
              :date [ui/html-select
                     {:fill true :value (or (:code number) "TT.MM.JJJJ")
                      :on-change #(swap! st update :format assoc-in [:number :code]
                                          (.. % -target -value))}
                     (for [c ["TT.MM.JJJJ" "TT.MM.JJ" "T. MMMM JJJJ"]]
                       ^{:key c} [:option {:value c} c])]
              :time [ui/html-select
                     {:fill true :value (or (:code number) "hh:mm")
                      :on-change #(swap! st update :format assoc-in [:number :code]
                                          (.. % -target -value))}
                     (for [c ["hh:mm" "hh:mm:ss"]] ^{:key c} [:option {:value c} c])]
              :custom [ui/input-group
                       {:value (or (:code number) "")
                        :placeholder "z. B. #.##0,00 \"Stk\";[Rot]-#.##0,00"
                        :on-change #(swap! st update :format assoc-in [:number :code]
                                           (.. % -target -value))}]
              nil)
            [:div {:class "np-muted"} "Formatcode: " [:code (or code "Standard")]]]
           [:section
            [:div {:class "np-section-title"} "Schrift"]
            (for [[k l] [[:bold "Fett"] [:italic "Kursiv"] [:underline "Unterstrichen"]]]
              ^{:key k}
              [ui/checkbox
               {:class "np-check" :label l
                :checked (boolean (get x k))
                :on-change #(swap! st update :format assoc k (.. % -target -checked))}])
            [color-field "Schriftfarbe" (:color x) #(swap! st update :format assoc :color %)]]
           [:section
            [:div {:class "np-section-title"} "Ausrichtung"]
            [ui/html-select
             {:fill true :value (or (:align x) "")
              :on-change #(swap! st update :format assoc :align (not-empty (.. % -target -value)))}
             [:option {:value ""} "Standard (Zahlen rechts, Text links)"]
             [:option {:value "left"} "Links"]
             [:option {:value "center"} "Zentriert"]
             [:option {:value "right"} "Rechts"]]
            [ui/checkbox
             {:class "np-check" :label "Zeilenumbruch"
              :checked (boolean (:wrap x))
              :on-change #(swap! st update :format assoc :wrap (.. % -target -checked))}]]
           [:section
            [:div {:class "np-section-title"} "Füllung"]
            [color-field "Hintergrund" (:fill x) #(swap! st update :format assoc :fill %)]
            [:div {:class "np-section-title"} "Schutz"]
            [ui/checkbox
             {:class "np-check"
              :label "Gesperrt — niemand ändert die Zelle, bis jemand die Sperre aufhebt"
              :checked (boolean (:locked? x))
              :on-change #(swap! st update :format assoc :locked? (.. % -target -checked))}]]]]]))))

(defn- sheet-of
  [m sid]
  (or (some #(when (= sid (:id %)) %) (:sheets m)) (first (:sheets m))))

(defn- locked [m s cs] (filter #(:lock (get-in m [:styles [(:id s) (ss/cell-ref %)]])) cs))

(defn- key-action
  [k ctrl? sh?]
  (let [lk (str/lower-case k)]
    (cond
      (and ctrl? (= "z" lk))          [(if sh? :redo :undo)]
      (and ctrl? (= "y" lk))          [:redo]
      (and ctrl? (#{"b" "i" "u"} lk)) [:toggle ({"b" :bold "i" :italic "u" :underline} lk)]
      (and ctrl? (= "1" k))           [:format]
      ctrl?                           nil
      (= "ArrowUp" k)                 [:move [-1 0] sh?]
      (= "ArrowDown" k)               [:move [1 0] sh?]
      (= "ArrowLeft" k)               [:move [0 -1] sh?]
      (= "ArrowRight" k)              [:move [0 1] sh?]
      (= "Tab" k)                     [:move [0 (if sh? -1 1)] false]
      (#{"Enter" "F2"} k)             [:edit nil]
      (#{"Delete" "Backspace"} k)     [:clear]
      (= 1 (count k))                 [:edit k])))

(defn- fresh-sheet-name [sheets]
  (let [taken (set (map (comp str/lower-case :name) sheets))]
    (some #(let [n (str "Tabelle" %)] (when-not (taken (str/lower-case n)) n))
          (iterate inc (inc (count sheets))))))

(defn- name-taken? [sheets s v]
  (some #(and (not= (:id %) (:id s)) (= (str/lower-case (:name %)) (str/lower-case v)))
        sheets))

(def ^:private style-keys [:bold :italic :underline :color :fill :align :wrap])

(defn- format-state
  [sty sel]
  (merge (select-keys sty style-keys)
         {:number  (format-choice (:format sty))
          :locked? (boolean (:lock sty))
          :range   (ss/range-label sel)}))

(defn- format-patch
  [{:keys [number] :as x}]
  (merge (zipmap style-keys (repeat nil))
         (into {} (filter (comp some? val)) (select-keys x style-keys))
         {:format (number-format number)}))

(defn- lock-stamp [user] {:by user :at (.toISOString (js/Date.))})

(defn- formatted
  [old patch {:keys [locked?]} lk? user]
  (when-not (and lk? locked?)
    (-> (merge old patch)
        (update-vals #(if (false? %) nil %))
        (assoc :lock (when locked? (or (:lock old) (lock-stamp user)))))))

(defn- sheet-styles [view-styles m sid]
  (merge view-styles (into {} (for [[[i a] sty] (:styles m) :when (= i sid)] [a sty]))))

(defn- grid-size
  [cells styles]
  (let [used (keep ss/cell-position (concat (keys cells) (keys styles)))]
    [(max 60 (+ 20 (reduce max 0 (map first used))))
     (max 16 (+ 4 (reduce max 0 (map second used))))]))

(defn- peers
  [^js aw ^js ydoc sid]
  (when aw
    (let [acc (atom {})]
      (.forEach (.getStates aw)
                (fn [^js x cid]
                  (when-let [^js c (.-cell x)]
                    (when (and (not= cid (.-clientID ydoc)) (= (.-sheet c) sid))
                      (swap! acc update (.-a1 c) (fnil conj [])
                             {:name     (some-> x .-user .-name)
                              :color    (or (some-> x .-user .-color) (live/color-of ""))
                              :editing? (.-editing c)})))))
      @acc)))

(defn- toast! [msg]
  (ui/show! (ui/default-toaster) {:message msg :intent :warning :timeout 3500}))

(defn- model [{:keys [ydoc base]}]
  (let [m (when ydoc (y-model ydoc))]
    (if (seq (:sheets m)) (assoc m :live? true) (or @base {:sheets []}))))

(defn- active
  [{:keys [st] :as c}]
  (let [m (model c)]
    [m (sheet-of m (get-in @st [:sel :sheet])) (ss/cell-ref (get-in @st [:sel :a]))]))

(defn- to! [{:keys [frame]} event] (machine/dispatch event {:frame frame}))

(machine/reg-event :notebook.sheet/calc
                   (fn [{:keys [db]} [_ id req on-body]]
                     (let [n (inc (:seq db 0))]
                       (rq/request (assoc db :seq n)
                                   {:ask     [api/post! :document-sheet {:id (str id)}
                                              {:sheets req :locale locale}]
                                    :current [[:seq] n] :error [:error]
                                    :then    [[:notebook.sheet/calced n on-body]]}))))

(machine/reg-event :notebook.sheet/calced
                   (fn [{:keys [db]} [_ n on-body res]]
                     (when (= n (:seq db))
                       (let [body (:body res)
                             new? (and on-body (not= body (:last-body db)))]
                         {:db (cond-> (assoc db :view (view-by-name (:sheets res)) :error nil)
                                new? (assoc :last-body body))
                          :fx [(when new? [:invoke [on-body body]])]}))))

(machine/reg-event :notebook.sheet/load
                   (fn [_ [_ id body on-loaded]]
                     {:fx [(rq/ask {:ask   [api/post! :document-sheet {:id (str id)}
                                            {:body (or body "") :locale locale}]
                                    :error [:error] :then [[:rf/invoke on-loaded]]})]}))

(defn- calc! [{:keys [id edit? on-body] :as c}]
  (let [m (model c)]
    (when (:live? m)
      (to! c [:notebook.sheet/calc id (request m) (when edit? on-body)]))))

(defn- schedule! [{:keys [timer] :as c}]
  (js/clearTimeout @timer)
  (reset! timer (js/setTimeout #(calc! c) calc-ms)))

(defn- writable!
  [{:keys [ydoc edit? base] :as c}]
  (when (and ydoc edit?)
    (when-not (:live? (model c))
      (when @base (seed! ydoc @base)))
    true))

(defn- publish! [{:keys [^js aw st] :as c}]
  (when aw
    (let [[_ s a] (active c)]
      (.setLocalStateField aw "cell" #js {:sheet (:id s) :a1 a :editing (boolean (:edit @st))}))))

(defn- select! [{:keys [st] :as c} rc extend?]
  (swap! st update :sel ss/select rc extend?)
  (publish! c))

(defn- move! [{:keys [st] :as c} step extend?]
  (swap! st update :sel ss/step step extend?)
  (publish! c))

(defn- start-edit! [{:keys [st edit?] :as c} text origin]
  (let [[m s a] (active c)]
    (cond
      (not edit?) nil
      (seq (locked m s [(get-in @st [:sel :a])]))
      (toast! (lock-title (:lock (get-in m [:styles [(:id s) a]]))))
      :else (do (swap! st assoc :edit {:text   (if (some? text) text
                                                   (get-in m [:cells [(:id s) a]] ""))
                                       :origin origin})
                (publish! c)))))

(def ^:private point-after #{\= \( \; \, \+ \- \* \/ \^ \& \< \>})

(defn- input-el [{:keys [st bar-el cell-el]}]
  (if (= :bar (get-in @st [:edit :origin])) @bar-el @cell-el))

(defn- caret [c text]
  (if-let [^js el (input-el c)] (.-selectionStart el) (count text)))

(defn- pointing? [{:keys [st] :as c}]
  (when-let [{:keys [text ref]} (:edit @st)]
    (or (some? ref)
        (and (str/starts-with? text "=")
             (contains? point-after (last (str/trimr (subs text 0 (caret c text)))))))))

(defn- point!
  [{:keys [st] :as c} rc extend?]
  (let [{:keys [text ref]} (:edit @st)
        ^js el (input-el c)
        [s e]  (if ref
                 [(:start ref) (:end ref)]
                 (if el [(.-selectionStart el) (.-selectionEnd el)] [(count text) (count text)]))
        anchor (if (and ref extend?) (:anchor ref) rc)
        t      (ss/range-label {:a anchor :b rc})
        p      (+ s (count t))]
    (swap! st update :edit assoc
           :text (str (subs text 0 s) t (subs text e))
           :ref {:start s :end p :anchor anchor :at rc})
    (r/after-render #(when-let [^js el (input-el c)] (.focus el) (.setSelectionRange el p p)))))

(defn- point-step! [{:keys [st] :as c} [dr dc] extend?]
  (let [[r col] (or (get-in @st [:edit :ref :at]) (get-in @st [:sel :a]))]
    (point! c [(max 0 (+ r dr)) (max 0 (+ col dc))] extend?)))

(defn- typed! [{:keys [st]} ^js e]
  (swap! st update :edit assoc :text (.. e -target -value) :ref nil))

(defn- commit! [{:keys [st ydoc root] :as c} step]
  (when-let [{:keys [text]} (:edit @st)]
    (when (writable! c)
      (let [[_ s a] (active c)] (set-input! ydoc (:id s) a text)))
    (swap! st assoc :edit nil)
    (some-> @root .focus)
    (when step (move! c step false))
    (publish! c)))

(defn- cancel! [{:keys [st root] :as c}]
  (swap! st assoc :edit nil)
  (some-> @root .focus)
  (publish! c))

(defn- clear! [{:keys [st ^js ydoc] :as c}]
  (let [[m s _] (active c)
        cs (cells-of (:sel @st))
        lk (set (locked m s cs))]
    (when (writable! c)
      (.transact ydoc #(doseq [rc cs :when (not (lk rc))]
                         (set-input! ydoc (:id s) (ss/cell-ref rc) nil))))
    (when (seq lk) (toast! (str (count lk) " gesperrte Zelle(n) blieben stehen")))))

(defn- restyle!
  [{:keys [st ^js ydoc] :as c} f]
  (let [[m s _] (active c)
        cs (cells-of (:sel @st))
        lk (set (locked m s cs))]
    (when (writable! c)
      (.transact ydoc
                 #(doseq [rc cs
                          :let [old (get-in (model c) [:styles [(:id s) (ss/cell-ref rc)]])
                                new (f old (lk rc))]
                          :when (not= old new)]
                    (set-style! ydoc (:id s) (ss/cell-ref rc) new))))))

(defn- toggle! [c k]
  (let [[m s a] (active c)
        on? (not (get-in m [:styles [(:id s) a] k]))]
    (restyle! c (fn [old lk?] (if lk? old (assoc old k (when on? true)))))))

(defn- lock! [{:keys [user] :as c} on?]
  (restyle! c (fn [old _] (assoc old :lock (when on? (lock-stamp user))))))

(defn- align! [c al current]
  (restyle! c (fn [old lk?] (if lk? old (assoc old :align (when-not (= al current) al))))))

(defn- number-format! [c v]
  (restyle! c (fn [old lk?] (if lk? old (assoc old :format v)))))

(defn- open-format! [{:keys [st edit?] :as c}]
  (let [[m s a] (active c)]
    (when edit?
      (swap! st assoc :format (format-state (get-in m [:styles [(:id s) a]]) (:sel @st))))))

(defn- apply-format! [{:keys [user] :as c} x]
  (let [patch   (format-patch x)
        skipped (atom 0)]
    (restyle! c (fn [old lk?]
                  (or (formatted old patch x lk? user)
                      (do (swap! skipped inc) old))))
    (when (pos? @skipped)
      (toast! (str @skipped " gesperrte Zelle(n) behielten ihr Format")))))

(defn- copy! [{:keys [st clip] :as c} ^js e]
  (when-not (:edit @st)
    (let [[m s _] (active c)
          area  (ss/selection-range (:sel @st))
          vw    (get-in @st [:view (:name s) :cells])
          shown (ss/range-tsv area #(or (get-in vw [(ss/cell-ref %) :text]) ""))]
      (reset! clip {:text   shown
                    :inputs (ss/range-tsv area
                                          #(or (get-in m [:cells [(:id s) (ss/cell-ref %)]]) ""))})
      (.setData (.-clipboardData e) "text/plain" shown)
      (.preventDefault e))))

(defn- paste! [{:keys [st clip ^js ydoc] :as c} ^js e]
  (when-not (:edit @st)
    (let [t (.getData (.-clipboardData e) "text/plain")
          t (if (= t (:text @clip)) (:inputs @clip) t)
          [m s _] (active c)]
      (.preventDefault e)
      (when (writable! c)
        (let [cs (ss/parse-tsv t (get-in @st [:sel :a]))
              lk (set (locked m s (map first cs)))]
          (.transact ydoc #(doseq [[rc x] cs :when (not (lk rc))]
                             (set-input! ydoc (:id s) (ss/cell-ref rc) x)))
          (when (seq lk)
            (toast! (str (count lk) " gesperrte Zelle(n) blieben stehen"))))))))

(defn- add-sheet! [{:keys [st ydoc] :as c}]
  (when (writable! c)
    (let [m   (model c)
          yid (subs (str (random-uuid)) 0 8)]
      (.set (ymap ydoc "sheets") yid
            (pr-str {:name (fresh-sheet-name (:sheets m))
                     :pos  (inc (reduce max 0 (map :pos (:sheets m))))}))
      (swap! st assoc :sel {:sheet yid :a [0 0] :b [0 0]}))))

(defn- by-name [m s] (some #(when (= (:name %) (:name s)) %) (:sheets m)))

(defn- rename-sheet! [{:keys [edit? ydoc] :as c} s]
  (when edit?
    (to! c [:notebook.editor/prompt
            {:title "Blatt umbenennen" :label "Name" :default (:name s)}
            (fn [v]
              (let [v (str/trim (str v))]
                (cond
                  (or (str/blank? v) (= v (:name s))) nil
                  (name-taken? (:sheets (model c)) s v)
                  (toast! (str "Ein Blatt „" v "“ gibt es schon"))
                  (writable! c)
                  (let [s' (by-name (model c) s)]
                    (.set (ymap ydoc "sheets") (:id s')
                          (pr-str {:name v :pos (:pos s')}))))))])))

(defn- drop-sheet-entries! [^js ydoc id]
  (let [p (str id "!")]
    (.transact ydoc
               (fn []
                 (.delete (ymap ydoc "sheets") id)
                 (doseq [k ["cells" "styles"]
                         :let [ym (ymap ydoc k)]
                         ck (js/Array.from (.keys ym))
                         :when (str/starts-with? ck p)]
                   (.delete ym ck))))))

(defn- drop-sheet! [{:keys [st edit? ydoc] :as c} s]
  (when (and edit? (> (count (:sheets (model c))) 1))
    (to! c [:notebook.editor/confirm
            {:title (str "Blatt „" (:name s) "“ löschen?") :body drop-note :danger? true}
            #(when (writable! c)
               (drop-sheet-entries! ydoc (:id (by-name (model c) s)))
               (swap! st assoc-in [:sel :sheet] nil))])))

(defn- run-key! [{:keys [^js undo] :as c} [action & args]]
  (case action
    :undo   (some-> undo .undo)
    :redo   (some-> undo .redo)
    :toggle (toggle! c (first args))
    :format (open-format! c)
    :move   (apply move! c args)
    :edit   (start-edit! c (first args) :cell)
    :clear  (clear! c)))

(defn- key-down! [{:keys [st] :as c} ^js e]
  (when-not (:edit @st)
    (when-let [action (key-action (.-key e) (or (.-ctrlKey e) (.-metaKey e)) (.-shiftKey e))]
      (.preventDefault e)
      (run-key! c action))))

(def ^:private arrow-steps
  {"ArrowUp" [-1 0] "ArrowDown" [1 0] "ArrowLeft" [0 -1] "ArrowRight" [0 1]})

(defn- edit-key! [{:keys [st] :as c} ^js e]
  (.stopPropagation e)
  (let [back? (.-shiftKey e)
        k     (.-key e)
        step  (arrow-steps k)]
    (cond
      (and step (= :cell (get-in @st [:edit :origin])) (pointing? c))
      (do (.preventDefault e) (point-step! c step back?))
      (= "Enter" k)  (do (.preventDefault e) (commit! c [(if back? -1 1) 0]))
      (= "Tab" k)    (do (.preventDefault e) (commit! c [0 (if back? -1 1)]))
      (= "Escape" k) (do (.preventDefault e) (cancel! c)))))

(defn- context
  [{:keys [id body live edit? user seed? on-seeded on-body]}]
  (let [ydoc (some-> live :ydoc)
        st   (r/atom {:sel {:sheet nil :a [0 0] :b [0 0]} :edit nil :view {} :tick 0
                      :last-body body})
        c    {:id id :body body :edit? edit? :user user :seed? seed? :on-seeded on-seeded
              :on-body on-body :ydoc ydoc :aw (some-> live :aw) :st st
              :frame (machine/frame st {:name :notebook-sheet :lifetime :view})
              :base (r/atom nil) :timer (atom nil)
              :drag? (atom false) :clip (atom nil) :root (atom nil)
              :bar-el (atom nil) :cell-el (atom nil)
              :undo (when ydoc (Y/UndoManager. #js [(ymap ydoc "sheets") (ymap ydoc "cells")
                                                    (ymap ydoc "styles")]))}]
    (assoc c
           :on-y        (fn [_ _] (swap! (:st c) update :tick inc) (schedule! c))
           :on-aw       (fn [_ _] (swap! (:st c) update :tick inc))
           :stop-drag   (fn [_] (reset! (:drag? c) false))
           :apply-format #(apply-format! c %))))

(defn- loaded! [{:keys [st base ydoc edit? seed? on-seeded] :as c} res]
  (reset! base (view->model (:sheets res)))
  (when-not (:live? (model c))
    (swap! st assoc :view (view-by-name (:sheets res))))
  (when (and ydoc edit? seed? (seed?) (not (:live? (model c))))
    (seed! ydoc @base)
    (when on-seeded (on-seeded)))
  (schedule! c)
  (publish! c))

(defn- mount! [{:keys [id body ydoc ^js aw on-y on-aw stop-drag] :as c}]
  (.addEventListener js/window "mouseup" stop-drag)
  (when ydoc
    (doseq [k ["sheets" "cells" "styles"]] (.observe (ymap ydoc k) on-y)))
  (some-> aw (.on "change" on-aw))
  (to! c [:notebook.sheet/load id body #(loaded! c %)]))

(defn- unmount! [{:keys [ydoc ^js aw ^js undo timer on-y on-aw stop-drag frame]}]
  (machine/leave! frame)
  (.removeEventListener js/window "mouseup" stop-drag)
  (js/clearTimeout @timer)
  (when ydoc
    (doseq [k ["sheets" "cells" "styles"]] (.unobserve (ymap ydoc k) on-y)))
  (some-> aw (.off "change" on-aw))
  (some-> undo .destroy)
  (when aw (.setLocalStateField aw "cell" nil)))

(defn- tool
  [tip props label]
  [ui/tooltip {:content tip}
   [ui/button (cond-> (merge {:minimal true :small true :aria-label tip} props)
                (keyword? label) (assoc :icon label))
    (when-not (keyword? label) label)]])

(def ^:private quick-formats
  [["" "Standard"] ["0,00" "Zahl 0,00"] ["#.##0,00" "Zahl 1.234,00"] ["#.##0,00 €" "Währung €"]
   ["0%" "Prozent"] ["TT.MM.JJJJ" "Datum"] ["@" "Text"]])

(defn- toolbar [{:keys [edit? ^js undo] :as c} act-st]
  (let [off? (not edit?)]
    [page/toolbar
     [tool "Rückgängig (Strg+Z)" {:disabled (not (and undo edit?)) :on-click #(.undo undo)} :undo]
     [tool "Wiederholen (Strg+Y)" {:disabled (not (and undo edit?)) :on-click #(.redo undo)} :redo]
     [divider]
     (for [[k t] [[:bold "Fett (Strg+B)"] [:italic "Kursiv (Strg+I)"]
                  [:underline "Unterstrichen (Strg+U)"]]]
       ^{:key k}
       [tool t {:active (get act-st k) :disabled off? :on-click #(toggle! c k)} k])
     [divider]
     (for [[al l] [["left" :align-left] ["center" :align-center] ["right" :align-right]]]
       ^{:key al}
       [tool ({"left" "Links" "center" "Zentriert" "right" "Rechts"} al)
        {:active   (= al (:align act-st))
         :disabled off?
         :on-click #(align! c al (:align act-st))}
        l])
     [divider]
     [ui/html-select
      {:minimal true :disabled off?
       :value (or (:format act-st) "")
       :on-change #(number-format! c (not-empty (.. % -target -value)))}
      (for [[v l] quick-formats]
        ^{:key v} [:option {:value v} l])
      (when-not ((set (map first quick-formats)) (or (:format act-st) ""))
        [:option {:value (:format act-st)} (:format act-st)])]
     [tool "Zellen formatieren (Strg+1)" {:disabled off? :on-click #(open-format! c)}
      "Zellen formatieren…"]
     [divider]
     (if (:lock act-st)
       [tool (lock-title (:lock act-st))
        {:active true :disabled off? :icon :lock :on-click #(lock! c false)}
        "Entsperren"]
       [tool "Gewählte Zellen sperren — niemand ändert sie versehentlich"
        {:disabled off? :icon :unlock :on-click #(lock! c true)}
        "Sperren"])
     (when off? [:span {:class "np-muted np-sheet__ro"} "nur lesen"])]))

(defn- formula-bar [{:keys [st edit? bar-el] :as c} sel edit input]
  [:div {:class "np-sheet__bar"}
   [:span {:class "np-sheet__name"} (ss/range-label sel)]
   [:span {:class "np-sheet__fx"} "fx"]
   [:input {:class "np-sheet__input" :value input :read-only (not edit?)
            :ref #(reset! bar-el %)
            :on-focus #(when (and edit? (not edit)) (start-edit! c nil :bar))
            :on-change #(typed! c %)
            :on-key-down #(edit-key! c %)
            :on-blur #(when (= :bar (:origin (:edit @st))) (commit! c nil))}]])

(defn- cell-view
  [{:keys [st root drag? cell-el] :as c} {:keys [r col cv sty in? act? ref? who edit]}]
  [:td {:class (str (when in? " is-in") (when act? " is-active")
                    (when ref? " is-ref")
                    (when (:lock sty) " is-locked")
                    (when (and cv (nil? (:input cv))) " is-spill"))
        :style (cond-> (cell-style sty cv)
                 (seq who) (assoc :box-shadow (str "inset 0 0 0 2px " (:color (first who)))))
        :title (cond (:lock sty) (lock-title (:lock sty))
                     (seq who) (str/join ", " (map :name who))
                     (and act? (:input cv) (str/starts-with? (:input cv) "=")) (:input cv))
        :on-mouse-down (fn [^js e]
                         (cond
                           (and act? (= :cell (:origin edit))) nil
                           (and edit (pointing? c))
                           (do (.preventDefault e)
                               (reset! drag? true)
                               (point! c [r col] (.-shiftKey e)))
                           :else
                           (do (when (and edit (not act?)) (commit! c nil))
                               (.preventDefault e)
                               (some-> @root .focus)
                               (reset! drag? true)
                               (select! c [r col] (.-shiftKey e)))))
        :on-mouse-enter #(when @drag?
                           (if (get-in @st [:edit :ref])
                             (point! c [r col] true)
                             (select! c [r col] true)))
        :on-double-click #(start-edit! c nil :cell)}
   (if (and act? (= :cell (:origin edit)))
     [:input {:class "np-sheet__cell-input" :auto-focus true
              :ref #(reset! cell-el %)
              :value (:text edit)
              :on-change #(typed! c %)
              :on-key-down #(edit-key! c %)
              :on-blur #(when (= :cell (:origin (:edit @st))) (commit! c nil))}]
     [:span {:class "np-sheet__text"} (:text cv)])
   (when (:lock sty) [:span {:class "np-sheet__lock"} "🔒"])
   (when (seq who)
     [:span {:class "np-sheet__who" :style {:background (:color (first who))}}
      (str (:name (first who)) (when (:editing? (first who)) " ✎"))])])

(defn- grid [{:keys [root] :as c} {:keys [sel edit cells styles others]}]
  (let [[nrows ncols] (grid-size cells styles)
        ref-sel       (when-let [{:keys [anchor at]} (:ref edit)] {:a anchor :b at})]
    [sheet/grid {:rows nrows :cols ncols :sel sel
                 :ref #(reset! root %)
                 :on-key-down #(key-down! c %) :on-copy #(copy! c %) :on-paste #(paste! c %)
                 :cell (fn [r col in?]
                         (let [a (ss/cell-ref [r col])]
                           [cell-view c {:r r :col col :cv (get cells a) :sty (get styles a)
                                         :in? in? :act? (= [r col] (:a sel))
                                         :ref? (ss/selected? ref-sel [r col])
                                         :who (get others a) :edit edit}]))}]))

(defn- sheet-tabs [{:keys [st edit?] :as c} m sid]
  [:div {:class "np-sheet__tabs"}
   [ui/tabs {:id :np-sheets :selected-tab-id sid
             :on-change (fn [id]
                          (swap! st assoc :sel {:sheet id :a [0 0] :b [0 0]})
                          (publish! c))}
    (for [x (:sheets m)]
      [ui/tab {:id (:id x) :on-double-click #(rename-sheet! c x)
               :title [ui/tooltip {:content (when edit? "Doppelklick benennt um")}
                       [:span (:name x)]]}
       (when (and edit? (= (:id x) sid) (> (count (:sheets m)) 1))
         [ui/tooltip {:content "Blatt löschen"}
          [ui/button {:minimal true :small true :icon :cross
                      :on-click (fn [e] (.stopPropagation e) (drop-sheet! c x))
                      :aria-label "Blatt löschen"}]])])
    (when edit?
      [ui/tooltip {:content "Neues Blatt"}
       [ui/button {:minimal true :small true :icon :plus
                   :on-click #(add-sheet! c) :aria-label "Neues Blatt"}]])]])

(defn editor
  [opts]
  (let [{:keys [st aw ydoc] :as c} (context opts)]
    (r/create-class
     {:display-name "notebook-sheet"
      :component-did-mount    (fn [_] (mount! c))
      :component-will-unmount (fn [_] (unmount! c))
      :reagent-render
      (fn [_]
        (let [{:keys [sel edit view tick error]} @st
              _      tick
              m      (model c)
              s      (sheet-of m (:sheet sel))
              sid    (:id s)
              vw     (get view (:name s))
              cells  (:cells vw)
              styles (sheet-styles (:styles vw) m sid)
              act    (ss/cell-ref (:a sel))]
          [:div {:class "np-sheet"}
           [toolbar c (get styles act)]
           [formula-bar c sel edit (if edit (:text edit) (get-in m [:cells [sid act]] ""))]
           (when error [:div {:class "np-sheet__error"} error])
           [grid c {:sel sel :edit edit :cells cells :styles styles
                    :others (peers aw ydoc sid)}]
           [sheet-tabs c m sid]
           (when (:format @st) [format-dialog st (:apply-format c)])]))})))

(defn static
  [{:keys [id body]}]
  ^{:key (hash body)} [editor {:id id :body body :edit? false}])
