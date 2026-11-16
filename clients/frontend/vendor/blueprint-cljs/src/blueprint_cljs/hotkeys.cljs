(ns blueprint-cljs.hotkeys
  "Hotkeys: combo parsing, KeyComboTag, the hotkeys dialog (opened with `?`) and
   `hotkeys-target`, which binds a list of hotkey configs while it is mounted.
   Config keys: `:combo` \"mod+s\", `:label`, `:group`, `:global`, `:on-key-down`,
   `:on-key-up`, `:allow-in-input`, `:disabled`, `:prevent-default`, `:stop-propagation`."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom.client :as rdc]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.dialog :as dialog]
            [blueprint-cljs.icon :as icon]))

;; ------------------------------------------------------------------ parsing

(def modifier-bits {"alt" 1 "ctrl" 2 "meta" 4 "shift" 8})

(defn mac?
  ([] (mac? nil))
  ([platform]
   (let [p (or platform (some-> js/navigator .-platform))]
     (boolean (and p (re-find #"Mac|iPod|iPhone|iPad" p))))))

(def config-aliases
  {"cmd" "meta" "command" "meta" "del" "delete" "esc" "escape" "escape" "escape"
   "minus" "-" "option" "alt" "plus" "+" "return" "enter" "win" "meta" "up" "arrowup"
   "left" "arrowleft" "down" "arrowdown" "right" "arrowright"})

(defn- alias-of [k]
  (if (= k "mod") (if (mac?) "meta" "ctrl") (get config-aliases k)))

(def shift-keys
  {"~" "`" "_" "-" "+" "=" "{" "[" "}" "]" "|" "\\" ":" ";" "\"" "'" "<" "," ">" "."
   "?" "/"})

(def ^:private modifier-key-names #{"Shift" "Control" "Alt" "Meta"})

(defn parse-key-combo
  "\"mod + shift + p\" → {:modifiers bits :key \"p\"}."
  [combo]
  (reduce
   (fn [acc piece]
     (when (= piece "")
       (throw (js/Error. (str "Failed to parse key combo \"" combo "\""))))
     (let [piece (or (alias-of piece) piece)]
       (cond
         (modifier-bits piece) (update acc :modifiers + (modifier-bits piece))
         (shift-keys piece)    (-> acc
                                   (update :modifiers + (modifier-bits "shift"))
                                   (assoc :key (shift-keys piece)))
         :else                 (assoc acc :key (str/lower-case piece)))))
   {:modifiers 0 :key nil}
   (str/split (str/lower-case (str/replace combo #"\s" "")) #"\+")))

(defn- key-from-code [e]
  (let [code (.-code e)]
    (when code
      (cond
        (str/starts-with? code "Key")   (str/lower-case (subs code 3))
        (str/starts-with? code "Digit") (str/lower-case (subs code 5))
        (#{"Space" "Delete"} code)      (str/lower-case code)))))

(defn- alt-modified-char? [k]
  (and k (= 1 (count k)) (let [c (.charCodeAt k 0)] (or (> c 127) (< c 32)))))

(defn get-key-combo
  "The combo a KeyboardEvent stands for, as `parse-key-combo` returns it."
  [e]
  (let [k     (.-key e)
        code  (key-from-code e)
        key   (when-not (modifier-key-names k)
                (cond
                  (#{"Space" "Delete"} (.-code e))                     code
                  (and (.-altKey e) (alt-modified-char? k) code)      code
                  (and (some-> (.-code e) (str/starts-with? "Digit")) code) code
                  :else (or (some-> k str/lower-case) code)))
        mods  (cond-> 0
                (.-altKey e)   (+ 1)
                (.-ctrlKey e)  (+ 2)
                (.-metaKey e)  (+ 4)
                (.-shiftKey e) (+ 8))
        key   (if (and (.-shiftKey e) (shift-keys k)) (shift-keys k) key)]
    {:modifiers mods :key key}))

(defn combo-matches [a b] (and (= (:modifiers a) (:modifiers b)) (= (:key a) (:key b))))

(defn normalize-key-combo
  [combo]
  (for [k (str/split (str/replace combo #"\s" "") #"\+")]
    (let [n (or (alias-of (str/lower-case k)) k)]
      (if (= n "meta") (if (mac?) "cmd" "ctrl") n))))

(defn text-input?
  [el]
  (when el
    (let [tag (some-> (.-tagName el) str/lower-case)]
      (or (= tag "textarea")
          (and (= tag "input")
               (not (#{"checkbox" "radio" "button" "submit" "reset" "range" "color" "file"}
                     (str/lower-case (or (.-type el) "text")))))
          (.-isContentEditable el)))))

;; ------------------------------------------------------------------ rendering

(def ^:private key-icons
  {"alt" [:key-option "Alt/Option key" true] "arrowdown" [:arrow-down "Down key"]
   "arrowleft" [:arrow-left "Left key"] "arrowright" [:arrow-right "Right key"]
   "arrowup" [:arrow-up "Up key"] "cmd" [:key-command "Command key" true]
   "ctrl" [:key-control "Control key" true] "delete" [:key-delete "Delete key"]
   "enter" [:key-enter "Enter key"] "meta" [:key-command "Command key" true]
   "shift" [:key-shift "Shift key" true]})

(def ^:private display-aliases
  {"arrowdown" "down" "arrowleft" "left" "arrowright" "right" "arrowup" "up"})

(defn- key-icon [k]
  (let [[i title mac-only?] (key-icons (str/lower-case k))]
    (when (and i (or (not mac-only?) (mac?))) [i title])))

(defn key-combo-tag
  "Props: `:combo`, `:minimal`, `:class`."
  [{:keys [combo minimal class]}]
  (let [ks (vec (map #(if (= 1 (count %)) (str/upper-case %) %) (normalize-key-combo combo)))
        n  (count ks)]
    (into [:span {:class (bp/cx (bp/cls "key-combo") class {(bp/cls "minimal") minimal})}]
          (map-indexed
           (fn [i k]
             (let [[ic title] (key-icon k)]
               (if minimal
                 (if ic
                   ^{:key i} [icon/icon {:icon ic :title title}]
                   (if (= i (dec n)) k (str k " + ")))
                 ^{:key i}
                 [:kbd {:class (bp/cx (bp/cls "key") {(bp/cls "modifier-key") ic})}
                  (when ic [icon/icon {:icon ic :title title}])
                  (get display-aliases (str/lower-case k) k)])))
           ks))))

(defn hotkey
  "One row of the hotkeys dialog. Props: `:label`, `:combo`, `:class`."
  [{:keys [label class] :as props}]
  [:div {:class (bp/cx (bp/cls "hotkey") class)}
   [:div {:class (bp/cls "hotkey-label")} label]
   [key-combo-tag (select-keys props [:combo :minimal])]])

(defn hotkeys-list
  "The grouped list (Blueprint `Hotkeys`): globals first, then by group."
  [{:keys [hotkeys class global-group-name] :or {global-group-name "Global"}}]
  (let [hs (->> hotkeys
                (map #(cond-> % (and (:global %) (nil? (:group %)))
                              (assoc :group global-group-name)))
                (sort-by (juxt #(if (:global %) 0 1) #(str (:group %)))))]
    (into [:div {:class (bp/cx (bp/cls "hotkey-column") class)}]
          (mapcat
           (fn [[i h prev]]
             (cond-> []
               (not= (:group h) (:group prev))
               (conj ^{:key (str "g" i)} [:h4 {:class (bp/cls "heading")} (:group h)])
               true
               (conj ^{:key i} [hotkey h])))
           (map vector (range) hs (cons nil hs))))))

(defn hotkeys-dialog
  "Props: dialog props plus `:hotkeys`, `:global-group-name`."
  [{:keys [hotkeys global-group-name class] :as props}]
  [dialog/dialog (merge {:title "Hotkeys"}
                        (dissoc props :hotkeys :global-group-name)
                        {:class (bp/cx (bp/cls "hotkey-dialog") class)})
   [dialog/dialog-body
    [hotkeys-list {:hotkeys hotkeys :global-group-name global-group-name}]]])

;; ------------------------------------------------------------------ registry + dialog

(defonce ^:private registry (r/atom {}))
(defonce ^:private dialog-state (r/atom {:open? false}))
(defonce ^:private dialog-root (atom nil))

(def show-dialog-key-combo "?")

(defn- dialog-view []
  [hotkeys-dialog {:is-open  (:open? @dialog-state)
                   :on-close #(swap! dialog-state assoc :open? false)
                   :hotkeys  (vec (remove :disabled (mapcat identity (vals @registry))))}])

(defn show-hotkeys-dialog! []
  (when-not @dialog-root
    (let [el (.createElement js/document "div")]
      (.appendChild (.-body js/document) el)
      (reset! dialog-root el)
      (rdc/render (rdc/create-root el) [dialog-view])))
  (swap! dialog-state assoc :open? true))

(defn hide-hotkeys-dialog! [] (swap! dialog-state assoc :open? false))

(defn- invoke! [hotkeys global? combo e callback]
  (let [in-input? (text-input? (.-target e))]
    (doseq [h hotkeys
            :when (= (boolean (:global h)) global?)
            :when (not (or (:disabled h) (and in-input? (not (:allow-in-input h)))))
            :when (combo-matches (parse-key-combo (:combo h)) combo)]
      (when (:prevent-default h) (.preventDefault e))
      (when (:stop-propagation h) (.stopPropagation e))
      (when-let [f (get h callback)] (f e)))))

(defn- on-global-keydown [e]
  (let [combo (get-key-combo e)]
    (if (and (not (text-input? (.-target e)))
             (seq @registry)
             (combo-matches (parse-key-combo show-dialog-key-combo) combo))
      (show-hotkeys-dialog!)
      (doseq [hs (vals @registry)] (invoke! hs true combo e :on-key-down)))))

(defn- on-global-keyup [e]
  (let [combo (get-key-combo e)]
    (doseq [hs (vals @registry)] (invoke! hs true combo e :on-key-up))))

(defonce ^:private listening? (atom false))

(defn- ensure-listening! []
  (when-not @listening?
    (reset! listening? true)
    (.addEventListener js/document "keydown" on-global-keydown)
    (.addEventListener js/document "keyup" on-global-keyup)))

(defonce ^:private ids (atom 0))

(defn hotkeys-target
  "Binds `:hotkeys` (configs) while mounted: global ones on the document, local ones
   when focus is inside the wrapper. Props: `:hotkeys`, `:tag-name` (:div), `:tab-index`
   (0), `:class`; the rest goes to the wrapper. Children: the target content."
  [_props & _children]
  (let [id (swap! ids inc)]
    (r/create-class
     {:display-name "blueprint-cljs.hotkeys-target"
      :component-did-mount
      (fn [this]
        (ensure-listening!)
        (swap! registry assoc id (vec (:hotkeys (second (r/argv this))))))
      :component-did-update
      (fn [this _]
        (swap! registry assoc id (vec (:hotkeys (second (r/argv this))))))
      :component-will-unmount
      (fn [_] (swap! registry dissoc id))
      :reagent-render
      (fn [props & children]
        (let [{:keys [hotkeys tag-name tab-index class] :or {tag-name :div tab-index 0}}
              props]
          (into [(keyword tag-name)
                 (merge (dissoc props :hotkeys :tag-name :tab-index :class)
                        {:class       class
                         :tab-index   tab-index
                         :on-key-down #(invoke! hotkeys false (get-key-combo %) % :on-key-down)
                         :on-key-up   #(invoke! hotkeys false (get-key-combo %) % :on-key-up)})]
                children)))})))
