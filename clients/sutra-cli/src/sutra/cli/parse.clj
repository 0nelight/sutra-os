(ns sutra.cli.parse
  (:require [clojure.string :as str]
            [sutra.cli.flags :as flags]
            [sutra.cli.json :as json]
            [sutra.cli.session :as session]
            [sutra.cli.table :as table]
            [sutra.cli.tree :as tree]
            [sutra.parser.step-verbs :as sv]))

(defn tokenize
  [line]
  (let [sb (StringBuilder.)]
    (loop [cs (seq line), toks [], in? false, q nil]
      (if (empty? cs)
        (cond-> toks in? (conj (.toString sb)))
        (let [c (first cs), more (rest cs)]
          (cond
            q            (if (= c q)
                           (recur more toks true nil)
                           (do (.append sb c) (recur more toks true q)))
            (#{\" \'} c) (recur more toks true c)
            (Character/isWhitespace c)
            (if in?
              (let [t (.toString sb)] (.setLength sb 0) (recur more (conj toks t) false nil))
              (recur more toks false nil))
            :else        (do (.append sb c) (recur more toks true nil))))))))

(defn- of-status [s] (filter #(= s (:status %)) table/verbs))

(def meta-verbs
  (into {} (for [{:keys [verb leaves]} (of-status :meta)
                 w (cons (name verb) (mapcat :aliases leaves))]
             [w verb])))

(def verb-aliases
  (into {} (for [{:keys [verb status leaves]} table/verbs
                 :when (not= :meta status)
                 a (mapcat :aliases leaves)
                 :when (not= a (name verb))]
             [a (name verb)])))

(def ^:private file-only-verbs (into #{} (map :verb) (of-status :file-only)))

(defn- schema-entries [verb]
  (let [args (some (fn [[v _ a]] (when (= v verb) a)) sv/steps)]
    (when (and (vector? args) (= :map (first args)))
      (filter vector? (rest args)))))

(defn- entry-opts [[_ o]] (when (map? o) o))

(defn- schema-field [[k & more :as e]]
  (cond-> (case (last more)
            :boolean {:key k :bool? true}
            :keyword {:key k :kw? true}
            :int     {:key k :int? true}
            {:key k})
    (:optional (entry-opts e)) (assoc :optional true)))

(defn- schema-pos [entries]
  (->> entries (filter (comp :pos entry-opts)) (sort-by (comp :pos entry-opts))
       (mapv schema-field)))

(defn- schema-flag [[k :as e]]
  (let [n (name k)]
    [(cond-> n (str/ends-with? n "?") (subs 0 (dec (count n))))
     (dissoc (schema-field e) :optional)]))

(defn- derive-spec
  [entries {:keys [variadic kv flags no-flags] :as spec} presets]
  (let [spec  (cond-> spec
                (and (not (:pos spec))
                     (seq (schema-pos entries))) (assoc :pos (schema-pos entries)))
        taken (into (set no-flags)
                    (concat (map :key (:pos spec)) (map :key (vals flags)) presets
                            (keep identity [(:key variadic) kv])))
        fl    (merge (into {} (comp (remove (comp taken first)) (map schema-flag)) entries) flags)]
    (cond-> (dissoc spec :flags) (seq fl) (assoc :flags fl))))

(def arg-specs
  (let [presets  (reduce (fn [m {:keys [verb preset]}] (update m verb into (keys preset)))
                         {} tree/command-tree)
        declared (into {} (keep (fn [{:keys [verb args]}] (when args [verb args]))) table/verbs)]
    (into {}
          (map (fn [verb]
                 (let [spec (get declared verb {})]
                   [verb (if (or (:post spec) (:step spec))
                           spec
                           (derive-spec (schema-entries verb) spec (presets verb)))])))
          (into (set (keys declared)) (remove file-only-verbs) sv/verbs))))

(def verbs
  (->> (concat (keys meta-verbs) (keys verb-aliases) (map name (keys arg-specs)))
       distinct sort vec))

(defn usage
  [word]
  (let [verb (get verb-aliases word word)]
    (if-let [spec (get arg-specs (keyword verb))]
      (let [pos (for [f (:pos spec)]
                  (if (:optional f)
                    (str "[" (name (:key f)) "]")
                    (str "<" (name (:key f)) ">")))
            var (when-let [v (:variadic spec)] [(str "<" (name (:key v)) ">…")])
            kv  (when (:kv spec) ["k=v…"])
            flg (for [[fname f] (sort-by key (:flags spec))]
                  (str "--" fname (when-not (:bool? f) (str " <" (name (:key f)) ">"))))]
        (str (str/join " " (concat [(name (keyword verb))] pos var kv flg))
             (some->> (:hint spec) (str "\n  "))))
      (if-let [leaf (tree/leaf-for-verb (keyword (get meta-verbs verb (keyword verb))))]
        (str (str/join " " (:path leaf))
             (when (:sum leaf) (str " — " (:sum leaf))))
        (when (tree/namespace-roots word)
          (str word " " (str/join "|" (tree/subcommands word))))))))

(defn- resolve1
  [sess tok]
  (if-let [vs (session/resolve-pronoun sess tok)]
    (if (= 1 (count vs)) [:one (first vs)] [:multi (vec vs)])
    [:one tok]))

(defn- coerce [field v]
  (cond
    (and (:kw? field) (string? v))  (keyword v)
    (and (:int? field) (string? v)) (or (parse-long v)
                                        (throw (ex-info (str "expected an integer, got " (pr-str v))
                                                        {:value v})))
    :else v))

(defn- kv-token? [spec t]
  (and (:kv spec) (re-find #"^[\w.-]+=" t) (not (str/starts-with? t "--"))))

(defn- verb-flags
  [spec]
  (into {} (for [[fname f] (:flags spec)] [fname (assoc f :key fname)])))

(defn- unknown-flag! [spec flag]
  (let [fname (subs flag 2)]
    (throw (ex-info (str "unknown flag " flag
                         (if-let [fs (seq (sort (keys (:flags spec))))]
                           (str " — this verb takes: " (str/join " " (map #(str "--" %) fs)))
                           " — this verb takes no flags")
                         (when (= "branch" fname)
                           (str "; --branch goes IN FRONT of the verb:"
                                " bin/sutra --branch <name> <verb…>")))
                    {:flag fname}))))

(defn- split-tokens
  [spec toks]
  (let [{:keys [opts words unknown] :as parsed} (flags/parse (verb-flags spec) {} toks)]
    (some->> (first unknown) (unknown-flag! spec))
    (flags/refuse-missing! parsed)
    {:pos   (vec (remove #(kv-token? spec %) words))
     :flags opts
     :kv    (into {} (comp (filter #(kv-token? spec %)) (map #(str/split % #"=" 2))) words)}))

(defn- resolve-one
  [sess f raw]
  (let [[_ v] (resolve1 sess raw)]
    (coerce f (if (and (:id? f) (vector? v)) (first v) v))))

(defn- positional-args
  [sess spec ptoks usg]
  (loop [fields (cond-> (vec (:pos spec))
                  (:variadic spec) (conj (assoc (:variadic spec) :variadic? true)))
         ptoks  ptoks
         acc    {}]
    (let [[f & more] fields
          t (first ptoks)]
      (cond
        (nil? f)
        (if (seq ptoks)
          (throw (ex-info (str "unexpected extra argument(s): " (str/join " " ptoks) @usg)
                          {:extra ptoks}))
          acc)

        (:variadic? f)
        (recur more [] (assoc acc (:key f)
                              (vec (mapcat #(let [[_ v] (resolve1 sess %)]
                                              (if (vector? v) v [v]))
                                           ptoks))))

        t             (recur more (rest ptoks) (assoc acc (:key f) (resolve-one sess f t)))
        (:optional f) (recur more ptoks acc)
        :else         (throw (ex-info (str "missing argument " (name (:key f)) @usg)
                                      {:field f}))))))

(defn- flag-args
  [sess spec flags]
  (into {} (for [[fname raw] flags
                 :let [f (get-in spec [:flags fname])]]
             [(:key f) (cond (:bool? f)    true
                             (vector? raw) (mapv #(resolve-one sess f %) raw)
                             :else         (resolve-one sess f raw))])))

(defn- list-value
  [sess raw]
  (let [inner (subs raw 1 (dec (count raw)))]
    (if (re-find #"^\s*[\"{]" inner)
      (vec (json/read-str raw))
      (mapv #(second (resolve1 sess %))
            (remove str/blank? (map str/trim (str/split inner #",")))))))

(defn- kv-args
  [sess kv]
  (reduce (fn [{:keys [fanout] :as acc} [k raw]]
            (let [k (keyword k)]
              (if (and (string? raw) (str/starts-with? raw "[") (str/ends-with? raw "]"))
                (assoc-in acc [:kv k] (list-value sess raw))
                (let [[tag v] (resolve1 sess raw)]
                  (when (and (= tag :multi) fanout)
                    (throw (ex-info "only one fan-out ($/range) argument allowed per action"
                                    {})))
                  (cond-> (assoc-in acc [:kv k] v)
                    (= tag :multi) (assoc :fanout [k v]))))))
          {:kv {} :fanout nil}
          kv))

(defn- build-args
  [sess verb spec {:keys [pos flags kv]}]
  (let [usg    (delay (some->> (usage (name verb)) (str "\n  usage: ")))
        args   (merge (positional-args sess spec pos usg) (flag-args sess spec flags))
        kvk    (:kv spec)
        {m :kv fanout :fanout} (when kvk (kv-args sess kv))]
    {:args   (cond-> args (seq m) (assoc kvk m))
     :fanout fanout}))

(defn- eval-subcommand
  [toks]
  (case (first toks)
    ("suites" "list" "ls") {:steps [[:eval {:op :suites}]]}
    "history" (let [suite (second toks)
                    flag  #(second (drop-while (partial not= %) toks))
                    by    (flag "--by")
                    exp   (flag "--experiment")]
                (when-not suite
                  (throw (ex-info (str "usage: eval history <suite> "
                                       "[--by model|version|experiment|ref|<parameter>] "
                                       "[--experiment <name>]")
                                  {})))
                {:steps [[:eval (cond-> {:op :history :suite suite}
                                  by  (assoc :by by)
                                  exp (assoc :experiment exp))]]})
    "save"    (if-let [file (second toks)]
                {:steps [[:eval {:op :save :suite file}]]}
                (throw (ex-info "usage: eval save <suite-file.edn>" {})))
    "cases"   (if-let [run (second toks)]
                {:steps [[:eval (cond-> {:op :cases :run run}
                                  (some #{"--failing"} toks) (assoc :failing? true))]]}
                (throw (ex-info "usage: eval cases <run-id> [--failing]" {})))
    "job"     (if-let [id (second toks)]
                {:steps [[:eval {:op :job :run id}]]}
                (throw (ex-info "usage: eval job <job-id>" {})))
    "compare" (let [runs (vec (rest toks))]
                (when-not (<= 2 (count runs) 4)
                  (throw (ex-info "usage: eval compare <run-a> <run-b> [<run-c> [<run-d>]]" {})))
                {:steps [[:eval {:op :compare :diff runs}]]})
    nil))

(defn- apply-preset
  [result preset]
  (if (and (seq preset) (:steps result))
    (update result :steps (fn [steps] (mapv (fn [[v a]] [v (merge a preset)]) steps)))
    result))

(defn- near-verbs
  [head]
  (->> tree/command-tree
       (keep (fn [{:keys [path verb]}]
               (let [flags (keys (:flags (get arg-specs verb)))
                     word  (set [head (str/replace head #"s$" "")])]
                 (cond (some word path)     (str/join " " (take 2 path))
                       (some #{head} flags) (str (str/join " " path) " … --" head)))))
       distinct
       (take 6)))

(defn- namespace-hint
  [head]
  (if-let [subs (and (tree/namespace-roots head) (seq (tree/subcommands head)))]
    (str " · did you mean: " head " " (str/join "|" subs) "?")
    (if-let [near (seq (near-verbs head))]
      (str " · did you mean: " (str/join " | " near) "?")
      "")))

(defn- dispatch-flat
  [sess head rest-toks]
  (cond
    (some #{"--help" "-h"} rest-toks)
    {:meta :usage :args [(get verb-aliases head head)]}

    :else
    (if-let [m (get meta-verbs head)]
      {:meta m :args (vec rest-toks)}
      (or
       (and (= head "eval") (eval-subcommand rest-toks))
       (let [canon (get verb-aliases head head)
             verb  (keyword canon)
             spec  (get arg-specs verb)]
         (cond
           (nil? spec)
           (if (contains? sv/verbs verb)
             (throw (ex-info (str "'" head "' is a runbook step — it has no one-shot line form; "
                                  "run it inside an .entr runbook (see `explain " head "`)")
                             {:verb head :file-only? true}))
             (throw (ex-info (str "unknown command '" head "' — try `help`" (namespace-hint head))
                             {:verb head})))

           :else
           (let [{:keys [args fanout]} (build-args sess verb spec (split-tokens spec rest-toks))
                 step-verb (or (:step spec) verb)]
             (cond
               (:post spec) {:steps [[step-verb ((:post spec) args)]]}
               fanout
               (let [[k vs] fanout]
                 {:steps (mapv (fn [v] [step-verb (assoc-in args [(:kv spec) k] v)]) vs)})
               :else {:steps [[step-verb args]]}))))))))

(defn- line-globals
  [toks]
  (let [{:keys [opts words] :as parsed} (flags/parse flags/line-globals {} toks)]
    (flags/refuse-missing! parsed)
    [opts words]))

(defn- handle-tree-hit
  [sess {:keys [leaf remaining]}]
  (if (nil? (:verb leaf))
    (throw (ex-info (str "`" (str/join " " (:path leaf)) "` is planned — not yet built"
                         (when (:sum leaf) (str " (" (:sum leaf) ")"))
                         ".")
                    {:planned (:path leaf)}))
    (let [[globals rest*] (line-globals remaining)]
      (cond-> (apply-preset (dispatch-flat sess (name (:verb leaf)) rest*) (:preset leaf))
        (seq globals) (assoc :globals globals)))))

(defn line->cmds
  [sess line]
  (let [toks (tokenize (str line))]
    (if (empty? toks)
      {:blank true}
      (if-let [hit (tree/match toks)]
        (handle-tree-hit sess hit)
        (let [head (first toks)
              [globals rest-toks] (line-globals (rest toks))]
          (cond-> (dispatch-flat sess head rest-toks)
            (seq globals) (assoc :globals globals)))))))
