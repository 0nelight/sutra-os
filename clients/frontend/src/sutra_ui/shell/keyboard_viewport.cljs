(ns sutra-ui.shell.keyboard-viewport)

(def ^:private textfeld
  "input:not([type=checkbox],[type=radio],[type=button],[type=file]),textarea,[contenteditable=true]")

(defn- aktives-feld []
  (let [e (.-activeElement js/document)]
    (when (and e (.matches e textfeld)) e)))

(defn- abgleichen! []
  (let [vv         (.-visualViewport js/window)
        ^js wurzel (.-documentElement js/document)
        stil       (.-style wurzel)]
    (if (aktives-feld)
      (do (.setProperty stil "--vv-h" (str (.-height vv) "px"))
          (.setProperty stil "--vv-top" (str (.-offsetTop vv) "px"))
          (.setAttribute wurzel "data-tastatur" "")
          (when (or (pos? (.-scrollY js/window)) (pos? (.-offsetTop vv)))
            (.scrollTo js/window 0 0)))
      (do (.removeProperty stil "--vv-h")
          (.removeProperty stil "--vv-top")
          (.removeAttribute wurzel "data-tastatur")))))

(defn- px [wert]
  (let [n (js/parseFloat wert)]
    (if (js/isNaN n) 0 n)))

(defn- in-sicht! [^js e oben?]
  (loop [^js s (.-parentElement e)]
    (when s
      (let [^js stil (js/getComputedStyle s)]
        (when (#{"auto" "scroll"} (.-overflowY stil))
          (let [f     (.getBoundingClientRect e)
                r     (.getBoundingClientRect s)
                ueber (- (.-top f) (.-top r) (px (.-scrollPaddingTop stil)))
                unter (+ (- (.-bottom f) (.-bottom r)) (px (.-scrollPaddingBottom stil)))]
            (cond
              (or oben? (neg? ueber)) (set! (.-scrollTop s) (+ (.-scrollTop s) ueber))
              (pos? unter)            (set! (.-scrollTop s) (+ (.-scrollTop s) unter))))))
      (recur (.-parentElement s)))))

(defn- feld-zeigen! []
  (when-let [^js e (aktives-feld)]
    (in-sicht! e (= "combobox" (.getAttribute e "role")))))

(defn installieren! []
  (when (and (exists? js/window.visualViewport)
             (.-matches (.matchMedia js/window "(pointer: coarse)")))
    (let [vv (.-visualViewport js/window)
          folgen! (fn [] (abgleichen!) (feld-zeigen!))]
      (.addEventListener vv "resize" folgen!)
      (.addEventListener vv "scroll" folgen!)
      (.addEventListener js/document "focusin" folgen!)
      (.addEventListener js/document "focusout" #(js/setTimeout abgleichen! 0)))))
