(ns sutra.cli.spec-dump
  (:require [sutra.parser.entr :as entr]
            [sutra.i18n.core :as i18n]
            [sutra.cli.hints :as hints]
            [sutra.cli.parse :as parse]
            [sutra.cli.tree :as tree]
            [sutra.cli.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.types.endpoint-verbs :as ev])
  (:import [java.io File PushbackReader]))

(declare map-entries)

(defn- map-schema?
  [x]
  (and (vector? x) (= :map (first x))))

(defn- render-type
  ([form] (render-type form 0))
  ([form depth]
   (cond
     (keyword? form) (name form)
     (symbol? form)  (str form)
     (fn? form)      (-> form class .getName
                         clojure.lang.Compiler/demunge
                         (str/split #"/") last)
     (not (vector? form)) (str form)
     :else
     (let [[op & args] form
           args (remove map? args)]
       (case op
         :or     (if (= (set args) #{:keyword :string})
                   "name"
                   (str/join "|" (map #(render-type % (inc depth)) args)))
         :enum   (str/join "|" (map #(if (keyword? %) (name %) (str %)) args))
         :vector (str "[" (render-type (first args) (inc depth)) "]")
         :map-of (str "{" (render-type (first args) (inc depth)) "→"
                      (render-type (second args) (inc depth)) "}")
         :tuple  (str "(" (str/join " " (map #(render-type % (inc depth)) args)) ")")
         :=      (str (first args))
         :map    (if (>= depth 2) "map"
                     (str "{" (str/join ", " (map #(name (first %)) (map-entries form))) "}"))
         :?      (render-type (first args) depth)
         (str op))))))

(defn- map-entries
  [form]
  (->> (rest form)
       (remove map?)))

(defn- arg-field
  [entry]
  (let [[k a b] entry
        opts    (when (map? a) a)
        type    (if opts b a)]
    {:key      k
     :optional? (boolean (:optional opts))
     :type     (render-type type)}))

(defn- verb-arg-form
  [[verb cat]]
  (let [tail (nth cat 2 nil)
        opt? (and (vector? tail) (= :? (first tail)))
        form (if opt? (second tail) tail)]
    {:verb verb :arg-optional? opt? :arg-form form}))

(defn- verb-spec
  [{:keys [verb arg-optional? arg-form]} line-spec endpoints]
  (let [args (when (map-schema? arg-form) (map arg-field (map-entries arg-form)))
        payload (when-not (map-schema? arg-form) (render-type arg-form))
        hint (hints/verb-hint verb)]
    (cond-> {:verb verb
             :arg-optional? arg-optional?}
      (seq args)      (assoc :args (vec args))
      payload         (assoc :payload payload)
      line-spec       (assoc :line line-spec)
      (seq endpoints) (assoc :endpoints (vec endpoints))
      (:theme hint)   (assoc :theme (:theme hint))
      (:purpose hint) (assoc :purpose (:purpose hint)))))

(def ^:private arg-specs parse/arg-specs)
(def ^:private meta-verbs parse/meta-verbs)
(def ^:private verb-aliases parse/verb-aliases)

(defn- line-shape
  [verb]
  (when-let [{:keys [pos variadic flags kv step]} (get arg-specs verb)]
    (let [pos-strs (map (fn [{:keys [key optional]}]
                          (let [b (name key)] (if optional (str "[" b "]") (str "<" b ">"))))
                        pos)
          var-str  (when variadic (str "<" (name (:key variadic)) "…>"))
          kv-str   (when kv "k=v…")
          flag-str (map (fn [[f {:keys [bool?]}]] (if bool? (str "--" f) (str "--" f " V")))
                        flags)
          usage    (->> (concat pos-strs [var-str kv-str] flag-str)
                        (remove nil?) (str/join " "))]
      (cond-> {:usage usage}
        step  (assoc :emits-verb step)
        (seq flags) (assoc :flags (mapv (comp str first) flags))))))

(defn- file-forms
  [file]
  (with-open [r (PushbackReader. (io/reader file))]
    (binding [*read-eval* false]
      (into [] (take-while #(not= ::eof %))
            (repeatedly #(read {:eof ::eof :read-cond :allow} r))))))

(defn- children [x]
  (if (map? x) (mapcat identity x) (seq x)))

(def ^:private keyword-calls
  '#{api/get! api/post! api/put! api/delete!})

(def ^:private byte-calls
  '#{tx/get-bytes! tx/post-bytes!})

(defn- literal-path
  [arg]
  (let [lit (if (and (seq? arg) (= 'str (first arg))) (second arg) arg)]
    (when (string? lit) (str/replace lit #"/$" ""))))

(defn- body-calls
  [form]
  (letfn [(walk [x]
            (cond
              (and (seq? x) (keyword-calls (first x)) (keyword? (second x)))
              (cons (str (second x)) (mapcat walk (nnext x)))

              (and (seq? x) (byte-calls (first x)) (literal-path (second x)))
              (cons (literal-path (second x)) (mapcat walk (nnext x)))

              (and (symbol? x) (= "api" (namespace x))) [(str x)]
              (coll? x)                                 (mapcat walk (children x))))]
    (distinct (walk form))))

(defn- file-methods
  [forms]
  (let [fns     (into {} (keep (fn [[head n :as f]]
                                 (when (and ('#{defn defn-} head) (symbol? n)) [n f])))
                      (filter seq? forms))
        names   (fn [form] (filter fns (filter symbol? (tree-seq coll? children form))))
        reached (fn [form]
                  (loop [todo (names form) seen #{}]
                    (if-let [[n & more] (seq todo)]
                      (if (seen n)
                        (recur more seen)
                        (recur (concat more (names (fns n))) (conj seen n)))
                      seen)))]
    (keep (fn [[head multi verb :as m]]
            (when (and (= 'defmethod head) (= 'step! multi)
                       (keyword? verb) (not= :default verb))
              (when-let [calls (seq (distinct (concat (body-calls m)
                                                      (mapcat (comp body-calls fns)
                                                              (sort-by str (reached m))))))]
                [verb (vec calls)])))
          (filter seq? forms))))

(defn- endpoint-map
  []
  (let [files (sort (.listFiles (io/file "clients/sutra-cli/src/sutra/cli/verbs")))]
    (into
     (into {} (map (fn [[v {:keys [id]}]] [v [(str id)]])) ev/cli-verbs)
     (mapcat (comp file-methods file-forms) files))))

(defn- assert-coverage!
  []
  (when-let [gap (seq (hints/coverage-gap))]
    (throw (ex-info (str "hints.edn is missing a :purpose for " (count gap)
                         " verb(s): " (str/join ", " (sort (map name gap)))
                         " — add them to clients/sutra-cli/resources/cli/hints.edn")
                    {:missing (vec gap)}))))

(defn- open-map?
  [form]
  (let [p (second form)]
    (not (and (map? p) (:closed p)))))

(defn- arg-field-raw
  [entry]
  (let [[k a b] entry
        opts    (when (map? a) a)]
    {:key k :optional? (boolean (:optional opts)) :subform (if opts b a)}))

(defn- pred->json
  [form]
  (case (-> ^Object form class .getName clojure.lang.Compiler/demunge
            (str/split #"/") last)
    ("int?" "integer?" "nat-int?" "pos-int?" "neg-int?") {:type "integer"}
    ("number?" "double?" "float?")                       {:type "number"}
    "boolean?"                                           {:type "boolean"}
    "string?"                                            {:type "string"}
    ("keyword?" "simple-keyword?" "qualified-keyword?")  {:type "string"}
    {}))

(declare form->json)

(def ^:private ^:dynamic *registry* {})

(def ^:private ^:dynamic *expanding* #{})

(defn- map-form->json
  [form]
  (let [fields (map arg-field-raw (map-entries form))]
    {:type       "object"
     :properties (into {} (for [{:keys [key subform]} fields]
                            [key (form->json subform)]))
     :required   (vec (for [{:keys [key optional?]} fields :when (not optional?)]
                        (name key)))
     :additionalProperties (open-map? form)}))

(defn- form->json
  [form]
  (cond
    (keyword? form)
    (case form
      :string  {:type "string"}
      :keyword {:type "string"}
      :boolean {:type "boolean"}
      :int     {:type "integer"}
      :double  {:type "number"}
      :map     {:type "object"}
      :nil     {:type "null"}
      (if (contains? *registry* form)
        (if (*expanding* form)
          {:type "object"}
          (binding [*expanding* (conj *expanding* form)]
            (form->json (get *registry* form))))
        {}))

    (fn? form)         (pred->json form)
    (map-schema? form) (map-form->json form)

    (vector? form)
    (let [[op & raw] form
          props (first (filter map? raw))
          args  (remove map? raw)]
      (case op
        :or         (let [branches (distinct (map form->json args))]
                      (if (= 1 (count branches))
                        (first branches)
                        {:anyOf (vec branches)}))
        :enum       {:type "string" :enum (mapv #(if (keyword? %) (name %) %) args)}
        (:vector :sequential :set) {:type "array" :items (form->json (first args))}
        :map-of     {:type "object"}
        (:maybe :?) (form->json (first args))
        :=          {:const (let [a (first args)] (if (keyword? a) (name a) a))}

        :schema     (binding [*registry* (merge *registry* (:registry props))]
                      (form->json (first args)))

        :multi      {:anyOf (mapv (comp form->json second) args)}

        :ref        (form->json (first args))
        {}))

    :else {}))

(defn- verb-tool
  [{:keys [verb arg-optional? arg-form]}]
  (let [hint   (hints/verb-hint verb)
        schema (cond
                 (map-schema? arg-form) (map-form->json arg-form)
                 (= :map arg-form)      {:type "object" :properties {}
                                         :additionalProperties true}
                 :else                  {:type "object"
                                         :properties {:payload (form->json arg-form)}
                                         :required   ["payload"]})
        schema (cond-> schema arg-optional? (assoc :required []))
        desc   (str (:purpose hint)
                    (when-let [w (:when hint)] (str "\n\nWhen: " w)))]
    (cond-> {:name         (name verb)
             :description  (if (str/blank? desc) "—" desc)
             :input_schema schema}
      (:theme hint)          (assoc :theme (name (:theme hint)))
      (seq (:see-also hint)) (assoc :next (mapv name (:see-also hint))))))

(declare tool-manifest*)

(def ^:private surface-locale
  :en)

(defn tool-manifest
  []
  (assert-coverage!)
  (let [vorher (i18n/locale)]
    (i18n/set-locale! surface-locale)
    (try (tool-manifest*)
         (finally (i18n/set-locale! vorher)))))

(defn- tool-manifest* []
  {:tools (->> entr/Step (drop 2)
               (filter vector?)
               (remove #(= :malli.core/default (first %)))
               (remove #(= :install (first %)))
               (map verb-arg-form)
               (map verb-tool)
               (sort-by :name)
               vec)
   :note
   (str "Generated from sutra.parser.entr/Step + clients/sutra-cli/resources/cli/hints.edn by "
        "sutra.cli.spec-dump/tool-manifest. One tool per CLI step verb; invoke a "
        "tool by sending its name + args as a [:verb args] command (line-"
        "REPL, an .ent :steps runbook, or an in-process step! loop) through "
        "entr/validate! -> sutra.cli.dispatch/step!. `next` = suggested follow-up "
        "verbs (the verb's curated see-also).")})

(defn catalog
  []
  (assert-coverage!)
  (let [eps      (endpoint-map)
        children (->> entr/Step (drop 2)
                      (filter vector?)
                      (remove #(= :malli.core/default (first %))))
        server   (->> children
                      (map verb-arg-form)
                      (map (fn [{:keys [verb] :as v}]
                             (verb-spec v (line-shape verb) (get eps verb))))
                      (sort-by :verb))]
    {:server-verbs (vec server)
     :meta-verbs   (->> meta-verbs (map (fn [[word action]] {:word word :action action}))
                        (sort-by :word) vec)
     :aliases      verb-aliases
     :tree         (vec (for [{:keys [key label]} tree/sections
                              :let
                              [ls (sort-by :path (filter #(= key (:section %)) tree/command-tree))]
                              :when (seq ls)]
                          {:section key
                           :label  label
                           :leaves (mapv (fn [l] {:cmd    (str/join " " (:path l))
                                                  :verb   (:verb l)
                                                  :sum    (:sum l)
                                                  :status (tree/status l)}) ls)}))
     :drift-invariant
     "sutra.cli.dispatch step! methods == sutra.parser.step-verbs/verbs, checked by platform.cli-tree-test."}))

(defn- fmt-args [args]
  (->> args
       (map (fn [{:keys [key optional? type]}]
              (str (name key) (when optional? "?") ":" type)))
       (str/join " ")))

(defn- fmt-endpoints [endpoints]
  (if (seq endpoints)
    (str/join ", " (map #(str "`" % "`") endpoints))
    "—"))

(defn- commands-table
  [{:keys [server-verbs]}]
  (str "| verb | purpose | line usage | validated args | sutra.wire.api calls |\n"
       "|---|---|---|---|---|\n"
       (str/join "\n"
                 (for [{:keys [verb purpose args payload line endpoints
                               arg-optional?]} server-verbs]
                   (str "| `" (name verb) "`"
                        " | " (or purpose "—")
                        " | " (if-let [u (:usage line)]
                                (str "`" (name verb) (when (seq u) (str " " u)) "`") "—")
                        " | " (cond
                                (seq args) (str (fmt-args args)
                                                (when arg-optional? "  _(all optional)_"))
                                payload    (str "_payload:_ " payload)
                                :else      "—")
                        " | " (fmt-endpoints endpoints)
                        " |")))))

(defn- endpoint-cheatsheet
  [{:keys [server-verbs]}]
  (str "| CLI verb | sutra.wire.api calls |\n|---|---|\n"
       (str/join "\n"
                 (for [{:keys [verb endpoints]} server-verbs]
                   (str "| `" (name verb) "` | " (fmt-endpoints endpoints) " |")))))

(defn- meta-table
  [{:keys [meta-verbs aliases]}]
  (str "| word | action |\n|---|---|\n"
       (str/join "\n" (for [{:keys [word action]} meta-verbs]
                        (str "| `" word "` | " action " |")))
       "\n\n**Aliases:** "
       (str/join ", " (for [[a t] aliases] (str "`" a "` → `" t "`")))))

(defn- tree-table
  [{:keys [tree]}]
  (str/join "\n\n"
            (for [{:keys [label leaves]} tree]
              (str "#### " label "\n\n"
                   "| command | summary | status |\n|---|---|---|\n"
                   (str/join "\n"
                             (for [{:keys [cmd sum status]} leaves]
                               (str "| `" cmd "` | " sum " | " (name status) " |")))))))

(def regions
  {"commands"  commands-table
   "endpoints" endpoint-cheatsheet
   "tree"      tree-table
   "meta"      meta-table})

(defn- help-md [{:keys [drift-invariant] :as cat}]
  (str
   "# sutra CLI — command reference\n\n"
   "> GENERATED by `sutra.cli.spec-dump` from `sutra.parser.entr/Step` + "
   "`sutra.cli.parse/arg-specs`.\n"
   "> Do not edit by hand — re-run `sutra spec dump` (`clojure -M:spec-dump`).\n"
   "> Invariant: " drift-invariant "\n\n"
   "## Connecting (which backend + auth)\n\n"
   "The CLI has no `--url` flag. It resolves its backend by this precedence "
   "(see `resolve-base`):\n\n"
   "```\n"
   "--base URL  >  --instance NAME  >  $SUTRA_HTTP_URL  >  $SUTRA_HTTP_PORT (dev)  >  the prod "
   "URL from "
   ".sutra-site.edn  >  a local sutra-solo, else http://localhost:8030\n"
   "```\n\n"
   "- **prod**: `sutra overview` — prod (the URL in `.sutra-site.edn`) is the DEFAULT; no prefix "
   "needed. Without that file the default is local.\n"
   "- **dev**: `. bin/ports.sh && sutra overview` — local dev; sourcing `bin/ports.sh` exports "
   "`$SUTRA_HTTP_PORT`, which wins over the prod default.\n"
   "- **auth**: auto-logs in as the default admin on the first server call (creds cached under "
   "`~/.sutra`); `--creds PATH` overrides.\n"
   "- **several instances**: `.sutra-site.edn :instances {name url}` names them; a leading "
   "`--instance <name>` aims one call, `sutra instances` lists them with their login state and "
   "`sutra instances login <name>` asks for the password at the terminal. Each host keeps its "
   "own credentials file under `.run/creds/`.\n\n"
   "Verbs whose **line usage** column below is `—` have no one-shot line form: "
   "they are runbook steps, run inside an `.entr` runbook, not typed directly.\n\n"
   "## Command tree (noun → verb)\n\n"
   "The discoverable surface: every command as a `noun verb` path, grouped by "
   "topic (`sutra.cli.tree`). The flat single-word verbs below remain permanent "
   "aliases — a tree path and its flat form dispatch identically. `status` "
   "≠ `live`: `file-only` = a runbook step (no line form), `planned` = a "
   "roadmap leaf not yet built.\n\n"
   (tree-table cat)
   "\n\n## Server verbs\n\n"
   "Each verb is one closed malli validator; args below are read out of it "
   "(`?` = optional). The last column lists every `sutra.wire.api` call in the "
   "verb's `step!` body, read as forms (all calls, not a guessed primary one).\n\n"
   (commands-table cat)
   "\n\n## Client-meta verbs (never reach the server)\n\n"
   (meta-table cat)
   "\n"))

(defn inject
  [text cat]
  (reduce
   (fn [[txt changed] [nm render]]
     (let [re (re-pattern (str "(?s)(<!-- spec-dump:" nm " BEGIN -->).*?(<!-- spec-dump:" nm
                               " END -->)"))]
       (if (re-find re txt)
         [(str/replace txt re (fn [[_ b e]] (str b "\n" (render cat) "\n" e)))
          (conj changed nm)]
         [txt changed])))
   [text []]
   regions))

(defn- inject-file! [f cat]
  (let [orig (slurp f)
        [out changed] (inject orig cat)]
    (when (seq changed)
      (spit f out)
      (println (str "✓ injected [" (str/join " " changed) "] → " f)))
    (seq changed)))

(def ^:private default-inject [])

(defn -main
  [& args]
  (i18n/set-locale! surface-locale)
  (let [inject-args (->> args (drop-while #(not= % "--inject")) (drop 1))
        positional  (take-while #(not= % "--inject") args)
        dir  (io/file (or (first positional) "clients/sutra-cli/surface"))
        _    (.mkdirs ^File dir)
        cat  (catalog)]
    (spit (io/file dir "HELP.md") (help-md cat))
    (let [tools (tool-manifest)]
      (spit (io/file dir "commands.tools.json") (with-out-str (json/pprint tools)))
      (println (str "✓ wrote " (count (:server-verbs cat)) " server verbs + "
                    (count (:meta-verbs cat)) " meta verbs · "
                    (count (:tools tools)) " LLM tools")))
    (println (str "  → " (io/file dir "HELP.md")))
    (println (str "  → " (io/file dir "commands.tools.json")))
    (doseq [f (concat default-inject inject-args)
            :when (.exists (io/file ^String f))]
      (inject-file! (io/file f) cat))))
