(ns sutra-ui.shell.menu-keys
  (:require [sutra-ui.ui.core :as ui]))

(def ^:private submenu-li (str "li." (ui/cls "submenu")))

(defn- own-items
  [^js ul]
  (->> (array-seq (.querySelectorAll ul "[role=menuitem]"))
       (filter (fn [^js el]
                 (and (identical? ul (.closest el "[role=menu]"))
                      (not= "true" (.getAttribute el "aria-disabled")))))
       vec))

(defn- step [items cur delta]
  (let [n (count items) i (.indexOf items cur)]
    (when (pos? n)
      (nth items (if (neg? i) (if (pos? delta) 0 (dec n)) (mod (+ i delta) n))))))

(defn- submenu-of [^js item]
  (some-> (.closest item "li") (.querySelector "[role=menu]")))

(defn- focus-first-of-submenu! [^js item tries]
  (if-let [sub (submenu-of item)]
    (some-> (first (own-items sub)) .focus)
    (when (pos? tries)
      (js/setTimeout #(focus-first-of-submenu! item (dec tries)) 50))))

(defonce ^:private typed (atom nil))

(defn- typeahead [items cur k]
  (let [now  (js/Date.now)
        {:keys [text at]} @typed
        text (str (if (and at (< (- now at) 700)) text "") (.toLowerCase k))
        _    (reset! typed {:text text :at now})
        i    (max 0 (.indexOf items cur))
        ring (concat (drop (if (= 1 (count text)) (inc i) i) items) items)]
    (some (fn [^js el]
            (when (.startsWith (.toLowerCase (.trim (.-textContent el))) text) el))
          ring)))

(defn- on-key-down [^js e]
  (let [^js t  (.-target e)
        k      (.-key e)
        ^js ul (or (.closest t "[role=menu]")
                   (when (= "ArrowDown" k) (.querySelector t "[role=menu]")))]
    (when ul
      (let [items (own-items ul)
            cur   (when (identical? ul (.closest t "[role=menu]"))
                    (.closest t "[role=menuitem]"))
            go!   (fn [^js el] (when el (.preventDefault e) (.focus el)))]
        (case k
          "ArrowDown" (go! (step items cur 1))
          "ArrowUp"   (go! (step items cur -1))
          "Home"      (go! (first items))
          "End"       (go! (peek items))
          "ArrowRight"
          (when (and cur (some-> (.closest cur "li") (.matches submenu-li)))
            (.preventDefault e)
            (focus-first-of-submenu! cur 10))
          "ArrowLeft"
          (when-let [^js li (.closest ul submenu-li)]
            (go! (.querySelector li "[role=menuitem]")))
          (when (and cur (= 1 (count k)) (not= " " k)
                     (not (or (.-ctrlKey e) (.-metaKey e) (.-altKey e))))
            (go! (typeahead items cur k))))))))

(defonce ^:private installed? (atom false))

(defn install! []
  (when-not @installed?
    (reset! installed? true)
    (.addEventListener js/document "keydown" on-key-down)))
