(ns ^:unit platform.purity-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]))

(def decl (edn/read-string (slurp "cores.edn")))

(def ^:private ambient
  '#{Instant/now java.time.Instant/now LocalDate/now LocalDateTime/now ZonedDateTime/now
     java.time.LocalDate/now java.time.LocalDateTime/now java.time.ZonedDateTime/now
     ZoneId/systemDefault java.time.ZoneId/systemDefault
     System/currentTimeMillis System/nanoTime System/getenv System/getProperty
     random-uuid UUID/randomUUID java.util.UUID/randomUUID})

(def ^:private io-syms
  '#{spit println print prn printf io/file io/writer io/output-stream io/delete-file
     io/make-parents future Thread/sleep shell/sh System/console System/exit})

(def ^:private state-ctors '#{atom volatile! ref agent})

(def ^:private forbidden-libs
  '#{next.jdbc clojure.java.jdbc clojure.java.shell babashka.process babashka.http-client
     org.httpkit.client org.httpkit.server})

(defn- sym [node]
  (when (and node (= :token (n/tag node))) (let [v (n/sexpr node)] (when (symbol? v) v))))

(defn- kids [node] (when (n/inner? node) (remove n/whitespace-or-comment? (n/children node))))

(defn- head [node] (when (and node (= :list (n/tag node))) (sym (first (kids node)))))

(defn- quoted-sym [node]
  (when (= :quote (n/tag node)) (let [v (n/sexpr node)] (when (symbol? (second v)) (second v)))))

(defn- libspec-names [x]
  (cond
    (symbol? x) [x]
    (and (sequential? x) (symbol? (first x)))
    (let [[lib & more] x]
      (if (or (empty? more) (keyword? (first more)))
        [lib]
        (mapcat (fn [s] (map #(symbol (str lib "." %)) (libspec-names s))) more)))
    :else []))

(defn- ns-info [forms]
  (when-let [nsf (first (filter #(= 'ns (head %)) forms))]
    (let [[_ nm & clauses] (n/sexpr nsf)
          reqs (for [c clauses :when (and (seq? c) (#{:require :require-macros :use} (first c)))
                     spec (rest c) lib (libspec-names spec)] lib)]
      {:ns nm :requires (set reqs) :form nsf})))

(defn- walk [node f]
  (when-not (#{:uneval :comment} (n/tag node))
    (f node)
    (doseq [k (kids node)] (walk k f))))

(defn- resource-slurp? [node]
  (let [hit (volatile! false)]
    (walk node #(when (= 'io/resource (sym %)) (vreset! hit true)))
    @hit))

(defn- resource-locals
  [top]
  (let [out (volatile! #{})]
    (walk top
          (fn [node]
            (when (#{'let 'let* 'when-let 'if-let 'when-some 'if-some} (head node))
              (let [bv (second (kids node))]
                (when (and bv (= :vector (n/tag bv)))
                  (doseq [[b init] (partition 2 (kids bv))]
                    (when (and (sym b) (resource-slurp? init))
                      (vswap! out conj (sym b)))))))))
    @out))

(defn- findings-of
  [forms ns-form]
  (let [out (volatile! [])
        add (fn [rule detail node]
              (vswap! out conj {:rule rule :detail (str detail) :line (:row (meta node))}))]
    (doseq [top forms :when (not (identical? top ns-form))]
      (when-let [h (head top)]
        (when (= 'defonce h) (add :global-state "defonce" top))
        (when (= 'def h)
          (let [v (last (kids top))]
            (when (state-ctors (head v)) (add :global-state (str "def … (" (head v) ")") top)))))
      (let [res-locals (resource-locals top)]
        (walk top
              (fn [node]
                (let [s (sym node)]
                  (cond
                    (ambient s) (add :ambient s node)
                    (io-syms s) (add :io s node)
                    (= 'alter-var-root s) (add :global-state s node)))
                (let [h (head node)]
                  (cond
                    (and (= 'slurp h) (not (resource-slurp? node))
                         (not (contains? res-locals (sym (second (kids node))))))
                    (add :io "slurp of a non-resource" node)
                    (= '.now h) (add :ambient "(.now …)" node)
                    (#{'requiring-resolve 'require} h)
                    (doseq [q (keep quoted-sym (rest (kids node)))]
                      (vswap! out conj {:rule ::requires
                                        :lib (if (namespace q) (symbol (namespace q)) q)}))))))))
    @out))

(defn- source-files [root]
  (->> (file-seq (io/file root))
       (filter #(re-find #"\.clj[cs]?$" (.getName ^java.io.File %)))
       (sort-by str)))

(defn read-ns [^java.io.File f]
  (let [forms (kids (p/parse-file-all f))
        info  (ns-info forms)]
    (when info
      (let [fs (findings-of forms (:form info))
            {dyn true own false} (group-by #(= ::requires (:rule %)) fs)]
        (assoc info :file (str f) :findings (vec own)
               :requires (into (:requires info) (map :lib) dyn))))))

(defn- glob-re [pat]
  (let [pat  (str pat)
        tail (str/ends-with? pat ".*")
        segs (str/split (if tail (subs pat 0 (- (count pat) 2)) pat) #"\.")]
    (re-pattern (str (str/join "\\." (map #(if (= "*" %) "[^.]+" (java.util.regex.Pattern/quote %))
                                          segs))
                     (when tail "\\..+")))))

(def ^:private glob-re* (memoize glob-re))

(defn- glob-match? [pat nm]
  (boolean (re-matches (glob-re* pat) (str nm))))

(defn- declared [k] (set (keys (get decl k))))

(defn core-of
  [{:keys [ns file]}]
  (some (fn [[core {:keys [roots namespaces]}]]
          (when (or (some #(str/starts-with? file (str % "/")) roots)
                    (some #(glob-match? % ns) namespaces))
            core))
        (:cores decl)))

(defn all-namespaces []
  (->> (:scan decl) (mapcat source-files) (keep read-ns)
       (map #(assoc % :core (core-of %)))))

(defn- ours? [lib] (some #(str/starts-with? (str lib) %) (:ours decl)))

(defn- inferred-pure
  [nss edges seams own-findings]
  (loop [p (into seams (comp (filter #(and (:core %) (not (edges (:ns %))))) (map :ns)) nss)]
    (let [p' (into p (for [{:keys [ns requires] :as x} nss
                           :when (and (not (p ns)) (not (edges ns)) (empty? (own-findings x))
                                      (not-any? forbidden-libs requires)
                                      (every? #(or (p %) (not (ours? %))) requires))]
                       ns))]
      (if (= p p') p (recur p')))))

(defn violations
  [nss]
  (let [by-ns  (into {} (map (juxt :ns identity)) nss)
        seams  (declared :seams)
        edges  (into (declared :edges) seams)
        memo   (declared :memo)
        own    (fn [{:keys [ns findings]}]
                 (remove #(and (memo ns) (= :global-state (:rule %))) findings))
        pure?  (inferred-pure nss edges seams own)
        judge  (fn [{:keys [requires] :as x}]
                 (concat
                  (own x)
                  (for [lib (sort requires)
                        :when (or (forbidden-libs lib) (and (ours? lib) (not (pure? lib))))]
                    {:rule :requires-impure
                     :detail (str lib (when-let [f (first (some-> (by-ns lib) own))]
                                        (str " (" (name (:rule f)) " " (:detail f) ")")))})))]
    (concat
     (for [{:keys [ns core] :as x} nss
           :when (and core (not (edges ns)))
           v (judge x)]
       (assoc v :ns ns :file (:file x)))
     (for [e (sort edges)
           :let [xs (filter #(= e (:ns %)) nss)]
           :when (and (some :core xs) (every? (comp empty? judge) xs))]
       {:ns e :rule :stale-edge :detail "pure now — drop it from :edges/:seams"})
     (for [m (sort memo)
           :let [x (by-ns m)]
           :when (or (nil? x) (not-any? #(= :global-state (:rule %)) (:findings x)))]
       {:ns m :rule :stale-memo :detail "holds no cache — drop it from :memo"}))))

(defn- show [{:keys [ns rule detail line]}]
  (str ns (when line (str ":" line)) "  " (name rule) "  " detail))

(deftest every-core-is-pure
  (let [vs (violations (all-namespaces))]
    (is (empty? vs) (str/join "\n" (cons "" (map show vs))))))

(defn- order-violations
  [why order nss]
  (let [rank (into {} (for [[i g] (map-indexed vector order) n g] [n i]))]
    (for [{:keys [ns requires]} nss
          :let [i (rank ns)]
          :when i
          lib (sort requires)
          :let [j (rank lib)]
          :when (and j (> j i))]
      {:ns ns :rule :layer :detail (str "requires " lib ", of a later group — " why)})))

(defn- edge-violations
  [{:keys [why from from-roots except to except-to allow]} nss]
  (let [under?  (fn [{:keys [file]} roots] (some #(str/starts-with? file (str % "/")) roots))
        any?    (fn [pats nm] (some #(glob-match? % nm) pats))
        allowed (set (mapcat val allow))
        edges   (set (for [{:keys [ns requires] :as x} nss
                           :when (and (or (any? from ns) (under? x from-roots))
                                      (not (any? except ns)))
                           lib requires
                           :when (and (any? to lib) (not (any? except-to lib)))]
                       [ns lib]))]
    (concat
     (for [[ns lib] (sort (remove allowed edges))]
       {:ns ns :rule :layer :detail (str "requires " lib " — " why)})
     (for [[ns lib] (sort (remove edges allowed))]
       {:ns ns :rule :stale-allow
        :detail (str lib " is no longer required — drop it from :allow")}))))

(defn layer-violations
  [nss]
  (mapcat (fn [{:keys [why order] :as rule}]
            (if order (order-violations why order nss) (edge-violations rule nss)))
          (:layers decl)))

(def ^:private kern-forbidden
  '#{atom volatile! ref agent binding with-redefs alter-var-root set! requiring-resolve
     resolve find-var intern})

(defn kern-violations []
  (for [f (source-files "libs/world/src")
        :let [forms (kids (p/parse-file-all f))]
        top forms
        hit (let [out (volatile! [])]
              (walk top
                    (fn [node]
                      (when-let [h (kern-forbidden (head node))]
                        (vswap! out conj (str "(" h " …)")))
                      (when (and (= :meta (n/tag node))
                                 (some #(= :dynamic (n/sexpr %)) (kids node)))
                        (vswap! out conj "^:dynamic"))))
              @out)]
    {:ns (str f) :rule :kern-state :detail hit}))

(deftest the-kern-holds-no-state
  (let [vs (kern-violations)]
    (is (empty? vs) (str/join "\n" (cons "" (map show vs))))))

(deftest no-forbidden-layer-edge
  (let [vs (layer-violations (all-namespaces))]
    (is (empty? vs) (str/join "\n" (cons "" (map show vs))))))

(defn module-of [tier-of nm]
  (let [[root m] (str/split (str nm) #"\.")]
    (when (and (= "sutra" root) (tier-of m)) m)))

(defn module-violations
  [nss]
  (let [{:keys [tiers allow]} (:modules decl)
        tier-of (into {} (for [[i ms] (map-indexed vector tiers) m ms] [(name m) i]))
        allowed (set (mapcat val allow))
        edges   (set (for [{:keys [ns requires]} nss
                           :let [a (module-of tier-of ns)] :when a
                           lib requires
                           :let [b (module-of tier-of lib)]
                           :when (and b (not= a b) (>= (tier-of b) (tier-of a)))]
                       [ns lib]))]
    (concat
     (for [[ns lib] (sort (remove allowed edges))
           :let [a (module-of tier-of ns) b (module-of tier-of lib)]]
       {:ns ns :rule :module
        :detail (str "requires " lib " — " a " (tier " (tier-of a) ") may only use modules"
                     " below it, " b " is on tier " (tier-of b))})
     (for [[ns lib] (sort (remove edges allowed))]
       {:ns ns :rule :stale-allow
        :detail (str lib " is no longer required — drop it from :modules :allow")}))))

(deftest no-module-points-up
  (let [vs (module-violations (all-namespaces))]
    (is (empty? vs) (str/join "\n" (cons "" (map show vs))))))

(defn facade-violations
  [nss]
  (let [{:keys [tiers open allow]} (:modules decl)
        tier-of (into {} (for [[i ms] (map-indexed vector tiers) m ms] [(name m) i]))
        open    (set (map name open))
        allowed (into (set (mapcat val allow)) (mapcat #(mapcat val (:allow %))) (:layers decl))
        http?   #(glob-match? 'sutra.*.http.* (str % ".x"))]
    (for [{:keys [ns requires]} (sort-by :ns nss)
          :let [a (module-of tier-of ns)]
          :when (and a (not= "serve" a))
          lib (sort requires)
          :let [b (module-of tier-of lib)]
          :when (and b (not= a b) (not (open b)) (not= lib (symbol (str "sutra." b ".api")))
                     (not (and (http? ns) (http? lib))) (not (allowed [ns lib])))]
      {:ns ns :rule :facade
       :detail (str "requires " lib " — another module reaches " b " through sutra." b ".api")})))

(deftest every-module-is-reached-through-its-api
  (let [vs (facade-violations (all-namespaces))]
    (is (empty? vs) (str/join "\n" (cons "" (map show vs))))))

(deftest every-declared-core-has-namespaces
  (let [nss (all-namespaces)]
    (doseq [core (keys (:cores decl))]
      (is (some #(= core (:core %)) nss) (str core " matches no namespace")))))
