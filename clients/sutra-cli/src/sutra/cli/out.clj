(ns sutra.cli.out
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [sutra.cli.json :as json]
            [sutra.cli.view :refer [cell flat? text]]
            [sutra.i18n.core :as i18n]))

(defn json?
  [ctx]
  (boolean (:json? ctx)))

(defn tty? [] (some? (System/console)))

(def ^:private colour-off? (atom false))

(defn no-colour!
  []
  (reset! colour-off? true))

(def ^:private colour-on-terminal?
  (delay (cond
           (System/getenv "NO_COLOR")    false
           (System/getenv "FORCE_COLOR") true
           :else (and (tty?) (not= "dumb" (System/getenv "TERM"))))))

(def ^:private sgr
  {:reset 0 :bold 1 :dim 39 :red 31 :green 32 :yellow 33
   :blue 34 :magenta 35 :cyan 36 :gray 39})

(defn paint
  [codes s]
  (if (and (not @colour-off?) @colour-on-terminal?)
    (let [ks (if (keyword? codes) [codes] codes)]
      (str "\u001b[" (str/join ";" (map sgr ks)) "m" s "\u001b[0m"))
    (str s)))

(defn mark
  [codes glyph msg] (str "  " (paint codes glyph) " " msg))

(defn kname [x] (cond (keyword? x) (name x) (some? x) (str x)))

(defn short-id
  [x]
  (let [s (str x)] (subs s 0 (min 8 (count s)))))

(defn fmt [pat x] (String/format java.util.Locale/US pat (object-array [x])))

(defn rpad [s n]
  (let [s (str s) d (- n (count s))]
    (if (pos? d) (str s (apply str (repeat d \space))) s)))

(defn- term-width
  []
  (or (some-> (System/getenv "COLUMNS") str/trim parse-long)
      (when (tty?)
        (try
          (let [{:keys [exit out]} (sh/sh "bash" "-c" "stty size </dev/tty 2>/dev/null")]
            (when (zero? exit)
              (some-> (second (str/split (str/trim out) #"\s+")) parse-long)))
          (catch java.io.IOException _ nil)))
      100))

(defn grid
  [items indent codes]
  (let [gap   2
        cellw (+ gap (reduce max 0 (map count items)))
        avail (max cellw (- (term-width) indent))
        ncols (max 1 (quot avail cellw))
        pad   (apply str (repeat indent \space))]
    (->> (partition-all ncols items)
         (map (fn [row] (str pad (str/join "" (map #(paint codes (rpad % cellw)) row)))))
         (str/join "\n"))))

(defn table-lines
  [indent cols rows]
  (let [ind    (apply str (repeat indent \space))
        cells  (mapv (fn [row] (mapv (fn [[_ f]] (str (f row))) cols)) rows)
        widths (mapv (fn [i [h]] (apply max (count h) (map #(count (nth % i)) cells)))
                     (range) cols)
        line   (fn [xs]
                 (str/trimr (str/join "  " (map #(format (str "%-" %1 "s") %2) widths xs))))]
    (cons (str ind (paint :bold (line (map first cols))))
          (map #(str ind (line %)) cells))))

(defn show-lines
  ([v] (show-lines v 2))
  ([v indent]
   (let [ind (apply str (repeat indent \space))]
     (cond
       (nil? v) []
       (and (sequential? v) (seq v) (every? map? v))
       (let [ks (into []
                      (comp (mapcat keys) (distinct)
                            (filter (fn [k] (every? #(flat? (get % k)) v)))) v)]
         (concat (table-lines indent (for [k ks] [(kname k) #(cell (get % k))]) v)
                 [(str ind (paint :dim (str (count v) " rows")))]))
       (map? v)
       (let [w (apply max 0 (map (comp count kname key) (filter (comp flat? val) v)))]
         (mapcat (fn [[k x]]
                   (if (flat? x)
                     [(str ind (paint :cyan (format (str "%-" w "s") (kname k))) "  "
                           (str/replace (text x) "\n"
                                        (str "\n" ind (apply str (repeat (+ w 2) \space)))))]
                     (cons (str ind (paint :bold (kname k))) (show-lines x (+ indent 2)))))
                 v))
       (sequential? v)
       (mapcat #(if (flat? %) [(str ind "· " (text %))] (show-lines % (+ indent 2))) v)
       :else [(str ind (text v))]))))

(defn show!
  ([v] (show! v 2))
  ([v indent] (run! println (show-lines v indent))))

(defmulti render
  (fn [view _data] view))

(defmethod render :default [_ data] (show-lines data))

(defn lines-of [x]
  (cond (nil? x) [] (string? x) [x] (sequential? x) (mapcat lines-of x) :else [(str x)]))

(defn- pick-fields
  [m paths]
  (reduce (fn [acc ks]
            (let [v (get-in m ks ::absent)]
              (if (= v ::absent) acc (assoc-in acc ks v))))
          {} paths))

(defn project-fields
  [fields m]
  (if (and (seq fields) (map? m))
    (let [paths (mapv #(mapv keyword (str/split % #"\.")) fields)]
      (if (and (sequential? (:rows m)) (not-any? #(contains? m (first %)) paths))
        {:rows (mapv #(pick-fields % paths) (:rows m))}
        (pick-fields m paths)))
    m))

(def ^:private escaped-sgr
  #"\\u001b\[[0-9;]*m")

(defn result-json
  ([x] (result-json nil x))
  ([fields x]
   (let [s (json/write-str (project-fields fields x))]
     (cond-> s (str/includes? s "\\u001b") (str/replace escaped-sgr "")))))

(defn emit!
  ([ctx data] (emit! ctx data :default))
  ([ctx data view]
   (when-not (keyword? view)
     (throw (ex-info "emit! takes the ctx, the data and a view keyword" {:view view})))
   (if (json? ctx)
     (println (result-json (:fields ctx) data))
     (run! println (lines-of (render view data))))
   nil))

(defn say!
  [ctx & xs]
  (binding [*out* (if (json? ctx) *err* *out*)] (apply println xs)))

(defn warn!
  [& xs]
  (binding [*out* *err*] (apply println xs)))

(defn fail!
  [& lines]
  (binding [*out* *err*] (run! println lines))
  1)

(defn progress!
  [& xs]
  (binding [*out* *err*] (apply print xs) (flush)))

(defn rows-lines
  [rows empty-msg]
  (if (empty? rows)
    [(str "  " empty-msg)]
    (show-lines (map-indexed #(into {:# (str "#" (inc %1))} %2) rows))))

(defn emit-rows!
  [ctx rows empty-msg]
  (if (json? ctx) (emit! ctx {:rows rows}) (run! println (rows-lines rows empty-msg)))
  {:kind :objects :rows rows})

(defn prompt-yes?
  [ctx msg]
  (cond
    (:yes? ctx)
    (do (say! ctx (str "  " msg " " (paint :dim (str "[" (i18n/t :cli/auto-yes) "]")))) true)

    (not (tty?))
    (do (warn! (str "  " (paint :yellow "⏸ ") msg " "
                    (paint :bold (i18n/t :cli/declined-no-terminal))
                    " " (paint :cyan (i18n/t :cli/pass-yes-in-script))))
        false)

    :else
    (do (print (str "  " msg " " (paint :bold (str "[" (i18n/t :cli/yes-no) "] ")))) (flush)
        (contains? #{"y" "Y" "yes"} (str/trim (or (read-line) ""))))))

(defn read-visible
  [label default]
  (when-let [con (System/console)]
    (let [in (.readLine con "  %s%s: "
                        (object-array [label (if (seq (str default)) (str " [" default "]") "")]))
          in (some-> in str/trim)]
      (if (seq in) in default))))

(defn read-secret
  [label]
  (when-let [con (System/console)]
    (let [chars (.readPassword con "%s"
                               (object-array
                                [(str "  " (i18n/t :cli/prompt-secret {:label label}) ": ")]))]
      (when chars (String. chars)))))

(def start-here
  [["project-map" :cli/sh-project-map]
   ["overview"    :cli/sh-overview]
   ["apps"        :cli/sh-apps]
   ["describe"    :cli/sh-describe]
   ["projects"    :cli/sh-projects]])

(defn start-here-lines
  [terse?]
  [(str "  " (paint :bold (i18n/t :cli/guide-start-here))
        "  " (paint :dim (i18n/t :cli/guide-start-here-sub)))
   (for [[v d] start-here]
     (str "    " (paint :cyan (rpad v 14)) " " (paint :dim (i18n/t d))))
   (when terse? (str "    " (paint :dim (i18n/t :cli/guide-pointer))))
   ""])

(defn print-start-here!
  ([ctx] (print-start-here! ctx false))
  ([ctx terse?] (run! #(say! ctx %) (lines-of (start-here-lines terse?)))))
