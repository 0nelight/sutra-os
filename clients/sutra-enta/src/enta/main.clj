(ns enta.main
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.capps.derived :as cd]
            [enta.file :as f]
            [enta.live :as live]))

(def ^:private colour?
  (delay (and (nil? (System/getenv "NO_COLOR"))
              (some? (System/console))
              (not= "dumb" (System/getenv "TERM")))))

(def ^:private sgr {:red 31 :green 32 :yellow 33 :blue 34 :cyan 36 :bold 1 :dim 2})

(defn- paint [k s] (if @colour? (str "\033[" (sgr k) "m" s "\033[0m") (str s)))

(defn- line [label v]
  (when (not-empty (str v))
    (println (str "  " (paint :bold (format "%-16s" label)) " " (paint :dim v)))))

(defn- head [s] (println (str "\n" (paint :cyan s))))

(def ^:private flag-keys
  #{:type :name :kind :label :op :values :expr :over :agg :of :by :pick :where :to
    :out :dir :live :json})

(defn- parse-args [args]
  (loop [[a & more] args, out {:pos []}]
    (cond
      (nil? a) out
      (str/starts-with? (str a) "--")
      (let [k (keyword (subs a 2))]
        (when-not (flag-keys k)
          (throw (ex-info (str "unbekannte Option: " a) {:exit 2})))
        (if (#{:live :json} k)
          (recur more (assoc out k true))
          (recur (rest more) (assoc out k (first more)))))
      :else (recur more (update out :pos conj a)))))

(defn- kw [s] (when (not-empty (str s)) (keyword (str s))))

(defn- operand
  [s]
  (let [s (str/trim (str s))]
    (if (re-matches #"-?\d+(\.\d+)?" s) (edn/read-string s) (keyword s))))

(defn- context
  [app {:keys [live dir]}]
  (let [base {:object-types (mapv keyword (f/object-types app))}]
    (if-not live
      base
      (if-let [c (live/ctx (or dir "."))]
        (merge base c)
        (do (println (paint :yellow (str "  (--live: " (live/why-not (or dir "."))
                                         " — Eigenschafts- und Link-Prüfung entfällt)")))
            base)))))

(defn- problems-of [app opts]
  (cd/explain (f/derived-module app) (context app opts)))

(defn- report-problems! [probs]
  (doseq [{:keys [type name error]} probs]
    (println (str "  " (paint :red "✗") " "
                  (clojure.core/name type) "." (clojure.core/name name) " — " error))))

(defn- describe [spec]
  (case (cd/kind-of spec)
    :aggregation (let [{:keys [over agg of by pick where]} spec]
                   (str (clojure.core/name (or agg :?))
                        (when of (str " " (clojure.core/name of)))
                        (when by (str " nach " (clojure.core/name by)))
                        (when pick (str " → " (clojure.core/name pick)))
                        " über " (clojure.core/name (or over :?))
                        (when where (str "  wenn " (pr-str where)))))
    :column-math (pr-str (:expression spec))
    "?"))

(defn- kind-label [spec]
  (case (cd/kind-of spec) :aggregation "Aggregation" :column-math "Spaltenrechnung" "?"))

(defn- print-derived! [app only probs]
  (let [module (f/derived-module app)
        by-nm  (into {} (map (juxt (juxt :type :name) identity)) probs)
        types  (if only [only] (mapv keyword (f/object-types app)))]
    (if (empty? types)
      (println (paint :dim "  (kein Objekttyp gebunden)"))
      (doseq [t types]
        (println (str "  " (paint :bold (clojure.core/name t))))
        (let [props (cd/properties module t)]
          (if (empty? props)
            (println (paint :dim "    (noch keine abgeleiteten Eigenschaften)"))
            (doseq [[nm spec] props]
              (println (str "    " (format "%-22s" (clojure.core/name nm))
                            (format "%-16s" (kind-label spec))
                            (paint :dim (describe spec))))
              (when-let [p (get by-nm [t nm])]
                (println (str "      " (paint :red "✗ ") (:error p)))))))))))

(defmulti ^:private run (fn [verb _app _path _opts] verb))

(defmethod run "overview" [_ app _ opts]
  (let [module (f/derived-module app)
        probs  (problems-of app opts)]
    (head "Modul")
    (line "name" (some-> (:name app) clojure.core/name))
    (line "titel" (get-in app [:layout :title]))
    (line "beschreibung" (:description app))
    (line "automatisch veröffentlichen" (when (:auto-publish app) "ja"))
    (head "Objekttypen")
    (doseq [t (f/object-types app)]
      (let [n (count (cd/properties module (keyword t)))]
        (println (str "  " (format "%-24s" (clojure.core/name t))
                      (paint :dim (if (pos? n) (str n " abgeleitet") ""))))))
    (when (empty? (f/object-types app)) (println (paint :dim "  (keiner gebunden)")))
    (head "Capabilities")
    (println (str "  " (paint :bold "Abgeleitete Eigenschaften")
                  "  " (paint :dim (str "(" (count (cd/all-properties module))
                                        ") — modulweit, nicht in der Ontologie"))))
    (print-derived! app nil probs)
    (if (seq probs) 1 0)))

(defmethod run "object-types" [_ app _ _]
  (doseq [t (f/object-types app)] (println (str "  " (clojure.core/name t))))
  0)

(defmethod run "derived" [_ app _ opts]
  (let [probs (problems-of app opts)]
    (print-derived! app (kw (:type opts)) probs)
    (if (seq probs) 1 0)))

(defmethod run "properties" [_ app _ opts]
  (let [t   (or (kw (:type opts)) (throw (ex-info "--type fehlt" {:exit 2})))
        ctx (context app opts)
        {:keys [native aggregation column-math]}
        (cd/referenceable (f/derived-module app) t (get-in ctx [:props t]))]
    (head (str "Verwendbar in einer Spaltenrechnung auf " (clojure.core/name t)))
    (if (seq native)
      (do (println (str "  " (paint :bold "eigene Eigenschaften")))
          (doseq [p native] (println (str "    " (clojure.core/name p)))))
      (println (paint :dim "  (eigene Eigenschaften unbekannt — ohne --live nicht gelesen)")))
    (when (seq aggregation)
      (println (str "  " (paint :bold "Aggregationen dieses Moduls")))
      (doseq [p aggregation] (println (str "    " (clojure.core/name p)))))
    (when (seq column-math)
      (println (str "  " (paint :bold "Spaltenrechnungen") " "
                    (paint :dim "— NICHT verwendbar")))
      (doseq [p column-math] (println (str "    " (paint :dim (clojure.core/name p))))))
    0))

(defn- spec-from-opts
  [{:keys [kind label op values expr over agg of by pick where]}]
  (let [k (or (kw kind) (cond (or expr op) :column-math (or over agg) :aggregation))]
    (case k
      :column-math
      (let [e (cond expr (edn/read-string expr)
                    op   (into [(keyword op)] (map operand (str/split (str values) #",")))
                    :else nil)]
        (cond-> {:kind :column-math :expression e} label (assoc :label label)))
      :aggregation
      (cond-> {:kind :aggregation :over (kw over) :agg (or (kw agg) :sum)}
        label (assoc :label label)
        of    (assoc :of (kw of))
        by    (assoc :by (kw by))
        pick  (assoc :pick (kw pick))
        where (assoc :where (edn/read-string where)))
      (throw (ex-info (str "--kind fehlt — :column-math (dann --op/--values oder --expr)"
                           " oder :aggregation (dann --over/--agg)")
                      {:exit 2})))))

(defmethod run "derived-add" [_ app path opts]
  (let [t    (or (kw (:type opts)) (throw (ex-info "--type fehlt" {:exit 2})))
        nm   (or (kw (:name opts)) (throw (ex-info "--name fehlt" {:exit 2})))
        spec (spec-from-opts opts)
        app' (f/put-module app (cd/put (f/derived-module app) t nm spec))
        probs (cd/explain (f/derived-module app') (context app' opts))
        mine  (filter #(and (= (:type %) t) (= (:name %) nm)) probs)]
    (if (seq mine)
      (do (report-problems! mine) 1)
      (do (f/write-enta! app' path)
          (println (str "  " (paint :green "✓") " " (clojure.core/name t) "."
                        (clojure.core/name nm) "  " (kind-label spec) "  "
                        (paint :dim (describe spec))))
          (report-problems! probs)
          0))))

(defmethod run "derived-drop" [_ app path opts]
  (let [t  (or (kw (:type opts)) (throw (ex-info "--type fehlt" {:exit 2})))
        nm (or (kw (:name opts)) (throw (ex-info "--name fehlt" {:exit 2})))]
    (when-not (get-in (f/derived-module app) [t nm])
      (throw (ex-info (str "gibt es nicht: " (clojure.core/name t) "."
                           (clojure.core/name nm)) {:exit 1})))
    (f/write-enta! (f/put-module app (cd/drop-property (f/derived-module app) t nm)) path)
    (println (str "  " (paint :green "✓") " entfernt: " (clojure.core/name t) "."
                  (clojure.core/name nm)))
    0))

(defmethod run "derived-rename" [_ app path opts]
  (let [t   (or (kw (:type opts)) (throw (ex-info "--type fehlt" {:exit 2})))
        nm  (or (kw (:name opts)) (throw (ex-info "--name fehlt" {:exit 2})))
        new (or (kw (:to opts))   (throw (ex-info "--to fehlt" {:exit 2})))]
    (when-not (get-in (f/derived-module app) [t nm])
      (throw (ex-info (str "gibt es nicht: " (clojure.core/name t) "."
                           (clojure.core/name nm)) {:exit 1})))
    (f/write-enta! (f/put-module app (cd/rename (f/derived-module app) t nm new)) path)
    (println (str "  " (paint :green "✓") " " (clojure.core/name nm) " → "
                  (clojure.core/name new)
                  (paint :dim "  (Spaltenrechnungen, die sie lesen, wurden mitgezogen)")))
    0))

(defmethod run "check" [_ app path opts]
  (let [door  (f/validate app path)
        probs (problems-of app opts)]
    (when door (println (str "  " (paint :red "✗ Grammatik") "\n" door)))
    (report-problems! probs)
    (when (and (nil? door) (empty? probs))
      (println (str "  " (paint :green "✓") " " path " ist installierbar")))
    (if (or door (seq probs)) 1 0)))

(defmethod run "pull" [_ _ app-name {:keys [out dir]}]
  (if-let [file (live/app-file (or dir ".") app-name)]
    (let [path (or out (str app-name ".enta"))]
      (f/write-enta! file path)
      (println (str "  " (paint :green "✓") " " app-name " → " path))
      0)
    (do (println (str "  " (paint :red "✗") " " (live/why-not (or dir "."))))
        2)))

(defmethod run :default [verb _ _ _]
  (throw (ex-info (str "unbekannter Befehl: " verb
                       " — overview · object-types · derived · properties · "
                       "derived-add · derived-drop · derived-rename · check")
                  {:exit 2})))

(def ^:private usage
  "enta <befehl> <datei.enta> [--optionen]

  pull <app> [--out <datei>]              die LIVE-App als `.enta` holen
  overview <datei>                        Modul, Objekttypen, Capabilities
  object-types <datei>
  derived <datei> [--type T]              die Derived-Properties dieses Moduls
  properties <datei> --type T             was eine Spaltenrechnung lesen darf
  derived-add <datei> --type T --name N
        Spaltenrechnung:  --op div --values produktiv,gesamt   (oder --expr '[:div :a :b]')
        Aggregation:      --over <link-seite> --agg sum --of <feld> [--where '[:= :jahr 2026]']
                          (--over nennt die SEITE — ihren API-Namen, Vorgabe der Link-Name;
                           sie trägt die Richtung, eine Richtungsoption gibt es nicht)
        beide:            [--label \"…\"] [--kind column-math|aggregation]
  derived-drop <datei> --type T --name N
  derived-rename <datei> --type T --name N --to M
  check <datei>

  --live      Eigenschaften und Links gegen die INSTANZ prüfen (Login aus `sutra login`)
  --dir <d>   der Checkout, dessen Instanz --live meint (Vorgabe: .)")

(defn -main [& args]
  (let [code (try
               (let [{:keys [pos] :as opts} (parse-args args)
                     [verb path] pos]
                 (if (or (nil? verb) (#{"help" "--help" "-h"} verb))
                   (do (println usage) 0)
                   (let [path (or path (throw (ex-info (if (= "pull" verb)
                                                          "welche App?" "welche Datei?")
                                                        {:exit 2})))]
                     (run verb (when-not (= "pull" verb) (f/read-enta path)) path opts))))
               (catch clojure.lang.ExceptionInfo e
                 (println (str "  " (paint :red "✗") " " (.getMessage e)))
                 (or (:exit (ex-data e)) 1))
               (catch Exception e
                 (println (str "  " (paint :red "✗") " " (.getMessage e)))
                 1))]
    (System/exit (or code 0))))
