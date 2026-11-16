(ns sutra.cli.json
  (:require [clojure.string :as str]))

(defn- resolve-or-nil
  [sym]
  (try (requiring-resolve sym) (catch Exception _ nil)))

(def ^:private cheshire-parse (resolve-or-nil 'cheshire.core/parse-string))
(def ^:private dj-read (when-not cheshire-parse (resolve-or-nil 'clojure.data.json/read-str)))

(when-not (or cheshire-parse dj-read)
  (throw
   (ex-info
    "sutra.cli.json: no JSON parser on this host (neither cheshire.core nor clojure.data.json)"
    {:host (if (System/getProperty "babashka.version") :babashka :jvm)})))

(defn read-str
  [s & {:keys [key-fn]}]
  (if cheshire-parse
    (cheshire-parse s (when (= key-fn keyword) keyword))
    (if key-fn (dj-read s :key-fn key-fn) (dj-read s))))

(def ^:private escapes
  {\" "\\\"" \\ "\\\\" \newline "\\n" \return "\\r" \tab "\\t"
   \formfeed "\\f" \backspace "\\b"})

(def ^:private unescaped #"[\"\\\x00-\x1f]")

(defn- escape
  ^String [^String s]
  (if-not (re-find unescaped s)
    s
    (str/replace s unescaped
                 (fn [^String m]
                   (let [c (.charAt m 0)]
                     (or (escapes c) (format "\\u%04x" (int c))))))))

(defn- write-key ^String [k]
  (str "\"" (escape (cond (keyword? k) (subs (str k) 1)
                          (string? k)  k
                          :else        (str k)))
       "\""))

(defn- scalar
  [x]
  (cond
    (nil? x)     "null"
    (string? x)  (str "\"" (escape x) "\"")
    (boolean? x) (str x)
    (integer? x) (str x)
    (float? x)   (str x)
    (keyword? x) (str "\"" (escape (subs (str x) 1)) "\"")
    (symbol? x)  (str "\"" (escape (str x)) "\"")
    (map? x)     nil
    (coll? x)    nil
    :else        (str "\"" (escape (str x)) "\"")))

(declare ^:private render)

(defn- render-entries
  [open close items depth]
  (if (empty? items)
    (str open close)
    (let [pad  (apply str (repeat (* 2 (inc depth)) \space))
          shut (apply str (repeat (* 2 depth) \space))]
      (str open "\n"
           (str/join ",\n" (map #(str pad %) items))
           "\n" shut close))))

(defn- render
  ^String [x ^long depth]
  (or (scalar x)
      (if (map? x)
        (render-entries "{" "}"
                        (map (fn [[k v]] (str (write-key k) ": " (render v (inc depth)))) x)
                        depth)
        (render-entries "[" "]" (map #(render % (inc depth)) x) depth))))

(def ^:private generate (resolve-or-nil 'cheshire.core/generate-string))

(def ^:private pretty
  (when generate
    (let [add (requiring-resolve 'cheshire.generate/add-encoder)
          as-str (requiring-resolve 'cheshire.generate/encode-str)]
      (doseq [c [Object java.util.Date clojure.lang.Ratio java.math.BigDecimal]]
        (add c (fn [x jg] (as-str (str x) jg))))
      ((requiring-resolve 'cheshire.core/create-pretty-printer)
       (assoc @(requiring-resolve 'cheshire.core/default-pretty-print-options)
              :indent-arrays? true :object-field-value-separator ": ")))))

(defn- as-render
  ^String [^String s]
  (let [s (-> s
              (.replace "[ ]\n" "[]\n") (.replace "[ ],\n" "[],\n")
              (.replace "{ }\n" "{}\n") (.replace "{ },\n" "{},\n"))
        s (case s "[ ]" "[]" "{ }" "{}" s)]
    (if-not (str/includes? s "\\u00")
      s
      (str/replace s #"(\\+)u00([0-9A-F]{2})"
                   (fn [[m bs hex]]
                     (if (odd? (count bs)) (str bs "u00" (str/lower-case hex)) m))))))

(defn write-str
  ^String [x]
  (if pretty (as-render (generate x {:pretty pretty})) (render x 0)))

(defn pprint
  [x]
  (println (write-str x)))
