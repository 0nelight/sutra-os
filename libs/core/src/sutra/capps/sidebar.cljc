(ns sutra.capps.sidebar
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.tree :as tree]))

(defn item-kind
  [item]
  (cond (contains? item :app)    :app
        (contains? item :action) :action
        (contains? item :url)    :url))

(defn app-names
  [sidebar]
  (into [] (comp (mapcat :items) (keep :app) (map ->kw)) sidebar))

(defn action-names
  [sidebar]
  (into [] (comp (mapcat :items) (keep :action) (map ->kw)) sidebar))

(defn encode
  [s]
  #?(:cljs (js/encodeURIComponent s)
     :clj  (-> (java.net.URLEncoder/encode (str s) "UTF-8") (str/replace "+" "%20"))))

(defn fill-url
  [template params {:keys [id type-name values]}]
  (let [whole? (re-matches #"\s*\{\{[^}]+\}\}\s*" template)
        value  (fn [nm]
                 (case nm
                   "objectId"     id
                   "objectTypeId" type-name
                   (get values (->kw (or (get params (keyword nm)) nm)))))]
    (str/replace template #"\{\{\s*([^}\s]+)\s*\}\}"
                 (fn [[_ nm]]
                   (let [x (value nm)]
                     (cond (nil? x) ""
                           whole?   (str x)
                           :else    (encode (str x))))))))

(def default-texts
  {:locked "Keine Berechtigung für diese App"})

(defn- label-of [{:keys [title icon]} fallback]
  (str (when (seq icon) (str icon " ")) (or (not-empty title) fallback)))

(defn- app-node
  [{:keys [app inputs mode] :as item} reachable? texts]
  (let [a     (some-> app ->kw name)
        label (label-of item a)
        jump  {:do :open-app :app a :with (or inputs {})}]
    (cond
      (not (reachable? a))
      {:widget :markdown :class "capp-ov-sidebar__locked"
       :text   (str "🔒 " label " — " (:locked texts))}

      (= :compact (->kw mode))
      {:widget :button :label (str label " ↗") :class "capp-ov-sidebar__compact"
       :on-click jump}

      :else
      {:kind     :section
       :title    label
       :class    "capp-ov-sidebar__app"
       :collapse true
       :tag      {:widget :button :label "↗" :class "capp-ov-sidebar__open" :on-click jump}
       :body     (cond-> {:widget :embed :app a}
                   (seq inputs) (assoc :inputs inputs))})))

(defn- action-node
  [{:keys [action] :as item}]
  (let [an    (->kw action)
        label (label-of item (name an))]
    {:kind     :section
     :title    label
     :class    "capp-ov-sidebar__action"
     :collapse true
     :body     {:widget :action-form :action an :label label}}))

(defn- url-node
  [{:keys [url params] :as item} obj]
  (let [label (label-of item url)
        href  (fill-url url params obj)]
    {:widget :markdown :class "capp-ov-sidebar__link"
     :text   (str "[" (str/replace label #"[\[\]]" "") " ↗](" href ")")}))

(defn node
  [sidebar obj {:keys [reachable? texts]}]
  (let [texts      (merge default-texts texts)
        reachable? (or reachable? (constantly true))
        groups     (for [g sidebar
                         :when (seq (:items g))]
                     {:kind  :section
                      :title (:title g)
                      :visibility (:visibility g)
                      :class "capp-ov-sidebar__group"
                      :items (vec (for [it (:items g)]
                                    (case (item-kind it)
                                      :app    (app-node it reachable? texts)
                                      :action (action-node it)
                                      :url    (url-node it obj))))})]
    (when (seq groups)
      {:kind    :rows
       :class   "capp-ov-sidebar"
       :box     {:w 320}
       :items   (vec groups)
       :actions (vec (for [g sidebar it (:items g) :when (:action it)]
                       (cond-> {:action (->kw (:action it))}
                         (seq (:parameters it)) (assoc :parameters (:parameters it)))))})))

(def ^:private lifted
  [:vars :scenario :module-inputs :module-emit :title :subtitle :fill? :device])

(defn with-sidebar
  [view side]
  (if (nil? side)
    view
    (let [own  (or (not-empty (vec (:actions view)))
                   (into [] (comp (filter #(and (:widget %) (:action %)))
                                  (map #(->kw (:action %))) (distinct)
                                  (map (fn [a] {:action a})))
                         (tree/nodes view)))
          seen (into #{} (map #(->kw (:action %))) own)]
      (-> (select-keys view lifted)
          (assoc :kind    :columns
                 :items   [(apply dissoc view :actions lifted) (dissoc side :actions)]
                 :actions (into own (remove #(seen (:action %))) (:actions side)))))))
