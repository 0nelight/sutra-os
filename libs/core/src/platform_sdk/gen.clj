(ns platform-sdk.gen
  (:require [sutra.kit.refusal :as refusal]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [platform-sdk.schema :as schema]))

(set! *warn-on-reflection* true)

(defn- reader-safe-symbol
  [kind nm]
  (let [s (schema/token nm)
        v (try (edn/read-string s) (catch RuntimeException _ ::unreadable))]
    (when-not (and (symbol? v) (nil? (namespace v)) (= s (str v)))
      (refusal/refuse! :refusal.platform-sdk/unspellable {:kind kind :name nm :spelling s}
                       {:kind-name (name kind)}))
    (symbol s)))

(defn- core-shadows
  [names]
  (let [core (ns-publics 'clojure.core)]
    (vec (sort (filter #(contains? core %) names)))))

(defn- ns-header
  [ns-sym names requires]
  (str "(ns " ns-sym
       (when-let [shadowed (seq (core-shadows names))]
         (str "\n  (:refer-clojure :exclude [" (str/join " " shadowed) "])"))
       "\n  (:require " (str/join "\n            " requires) "))"))

(defn- ns-file
  [ns-sym target]
  (str (-> (str ns-sym) (str/replace "-" "_") (str/replace "." "/"))
       (if (= :cljs target) ".cljs" ".clj")))

(defn- doc-lines
  [& lines]
  (str "\""
       (-> (str/join "\n   " (remove nil? lines))
           (str/replace "\\" "\\\\")
           (str/replace "\"" "\\\""))
       "\""))

(defn- attr-line
  [spec]
  (when-let [attrs (seq (:attributes spec))]
    (str "Attributes: "
         (str/join " · " (for [[a t] attrs] (str a " " (schema/token t)))))))

(defn- link-line
  [sides]
  (when (seq sides)
    (str "Links (os/around set <side>): "
         (str/join " · " (for [[side info] sides]
                           (str side " → " (:far-type info)
                                " (" (schema/token (:cardinality info)) ")"))))))

(defn- type-form
  [ontology-ns [type-kw spec]]
  (let [sym (reader-safe-symbol :object-type type-kw)]
    (str "(defn " sym "\n  "
         (doc-lines (str "Object set of " type-kw ".")
                    (when-let [pk (:primary-key spec)] (str "Primary key " pk))
                    (attr-line spec)
                    (link-line (:links spec)))
         "\n  ([] (" sym " nil))\n"
         "  ([client] (os/of client " type-kw " " ontology-ns "/index)))")))

(defn- param-doc
  [label params]
  (when (seq params)
    (str label ": "
         (str/join " · " (for [[p s] params]
                           (str (keyword (schema/token p)) " " (schema/token (:type s))
                                (when (:multiple s) " (multiple)")))))))

(defn- action-form
  [nm spec]
  (let [sym      (symbol (str (reader-safe-symbol :action nm) "!"))
        params   (schema/action-params spec)
        required (vec (for [[p s] params :when (:required s)] (keyword (schema/token p))))
        all      (mapv (comp keyword schema/token key) params)]
    (str "(defn " sym "\n  "
         (doc-lines (or (:description spec) (:display-name spec)
                        (str "Action " (keyword (schema/token nm)) "."))
                    (param-doc "Required" (for [[p s] params :when (:required s)] [p s]))
                    (param-doc "Optional" (for [[p s] params :when (not (:required s))] [p s]))
                    "Options: {:publish? true} · {:stage? true} · {:dry-run? true}")
         "\n  ([args] (" sym " nil args nil))\n"
         "  ([client args] (" sym " client args nil))\n"
         "  ([client args opts]\n"
         "   (w/action! client " (pr-str (keyword (schema/token nm))) " args opts\n"
         "              {:required " (pr-str required) " :params " (pr-str all) "})))")))

(defn- function-form
  [spec edit?]
  (let [nm     (schema/function-name spec)
        base   (reader-safe-symbol :function nm)
        sym    (if edit? (symbol (str base "!")) base)
        params (schema/function-params spec)
        ks     (mapv (comp keyword schema/token key) params)]
    (str "(defn " sym "\n  "
         (doc-lines (or (:description spec)
                        (str (if edit? "Edit" "Query") " function "
                             (keyword (schema/token nm)) "."))
                    (when (seq params)
                      (str "Inputs: "
                           (str/join " · " (for [[p t] params]
                                             (str (keyword (schema/token p)) " "
                                                  (schema/token t))))))
                    (when (empty? params)
                      "Inputs: untyped — the ontology declares no parameter list."))
         "\n  ([inputs] (" sym " nil inputs))\n"
         "  ([client inputs]\n"
         "   (w/function! client " (pr-str (keyword (schema/token nm))) " inputs"
         (if (seq ks) (str "\n                {:params " (pr-str ks) "})))") ")))"))))

(defn- table-literal
  [value]
  (let [text (binding [pprint/*print-right-margin* 100
                       pprint/*print-miser-width* 80]
               (with-out-str (pprint/pprint value)))]
    (->> (str/split-lines (str/trim-newline text))
         (map #(str "  " %))
         (str/join "\n"))))

(defn- ontology-source
  [ns-sym {:keys [ontology fingerprint types rids]}]
  (str/join
   "\n\n"
   [(str ";; AUTO-GENERATED by platform-sdk.gen — do not edit by hand.\n"
         ";; Ontology \"" ontology "\", fingerprint " fingerprint ".\n"
         (ns-header ns-sym #{} ["[platform-sdk.client :as client]"
                               "[platform-sdk.errors :as err]"
                               "[platform-sdk.schema :as schema]"]))
    (str "(def ontology " (pr-str ontology) ")")
    (str "(def ontology-fingerprint " (pr-str fingerprint) ")")
    (str "(def object-types\n" (table-literal types) ")")
    (str "(def rids\n" (table-literal rids) ")")
    "(def index {:object-types object-types :rids rids})"
    (str "(defn client\n  "
         (doc-lines "A client for this ontology."
                    "{:base-url \"https://host\" :token \"…\"} or {:base-url … :username … :password …}.")
         "\n  [opts] (client/client opts))")
    (str "(defn verify\n  ([] (verify nil))\n"
         "  ([client]\n"
         "   (let [live (schema/fingerprint (client/schema client true))]\n"
         "     {:ok (= live ontology-fingerprint)\n"
         "      :live live\n"
         "      :generated-against ontology-fingerprint})))")
    (str "(defn verify!\n  ([] (verify! nil))\n"
         "  ([client]\n"
         "   (let [{:keys [ok live]} (verify client)]\n"
         "     (when-not ok\n"
         "       (err/raise! :drift\n"
         "                   (str \"ontology drift: generated against \" ontology-fingerprint\n"
         "                        \", the instance now serves \" live)\n"
         "                   {:generated-against ontology-fingerprint :live live}))\n"
         "     true)))")]))

(defn- cljs-ontology-source
  [ns-sym {:keys [ontology fingerprint types rids]}]
  (str/join
   "\n\n"
   [(str ";; AUTO-GENERATED by platform-sdk.gen — do not edit by hand.\n"
         ";; Ontology \"" ontology "\", fingerprint " fingerprint ".\n"
         (ns-header ns-sym #{} ["[platform-sdk.client :as client]"]))
    (str "(def ontology " (pr-str ontology) ")")
    (str "(def ontology-fingerprint " (pr-str fingerprint) ")")
    (str "(def object-types\n" (table-literal types) ")")
    (str "(def rids\n" (table-literal rids) ")")
    "(def index {:object-types object-types :rids rids})"
    (str "(defn client\n  "
         (doc-lines "A client for this ontology; every call answers a Promise."
                    "{:base-url \"https://host\" :client-id … :client-secret …} or {:base-url … :token …}.")
         "\n  [opts] (client/client opts))")]))

(defn- objects-source
  [ns-sym ontology-ns types]
  (str/join
   "\n\n"
   (concat
    [(str ";; AUTO-GENERATED by platform-sdk.gen — do not edit by hand.\n"
          (ns-header ns-sym
                     (set (map (comp symbol schema/token key) types))
                     [(str "[" ontology-ns " :as ontology]")
                      "[platform-sdk.objectset :as os]"]))]
    (for [entry types] (type-form 'ontology entry)))))

(defn- actions-source
  [ns-sym actions]
  (str/join
   "\n\n"
   (concat
    [(str ";; AUTO-GENERATED by platform-sdk.gen — do not edit by hand.\n"
          (ns-header ns-sym
                     (set (map #(symbol (str (schema/token %) "!")) (keys actions)))
                     ["[platform-sdk.wire :as w]"]))]
    (for [nm (sort-by schema/token (keys actions))]
      (action-form nm (get actions nm))))))

(defn- functions-source
  [ns-sym fns edit?]
  (str/join
   "\n\n"
   (concat
    [(str ";; AUTO-GENERATED by platform-sdk.gen — do not edit by hand.\n"
          (ns-header ns-sym
                     (set (for [spec fns]
                            (symbol (cond-> (schema/token (schema/function-name spec))
                                      (edit? spec) (str "!")))))
                     ["[platform-sdk.wire :as w]"]))]
    (for [spec (sort-by (comp schema/token schema/function-name) fns)]
      (function-form spec (edit? spec))))))

(defn- read-all!
  [path src]
  (let [rdr (java.io.PushbackReader. (java.io.StringReader. src))]
    (try
      (doall (take-while #(not= ::eof %)
                         (repeatedly #(read {:eof ::eof :read-cond :allow} rdr))))
      (catch Exception e
        (throw (ex-info (str "generated " path " does not read as Clojure: " (.getMessage e))
                        {:path path} e))))
    src))

(defn generate
  [schema-map {:keys [ns ontology backing target] :or {ontology "sutra" target :clj}}]
  (let [ns-sym      (symbol (str ns))
        objects-ns  (symbol (str ns-sym ".objects"))
        actions-ns  (symbol (str ns-sym ".actions"))
        fns-ns      (symbol (str ns-sym ".functions"))
        _           (doseq [[rid spec] (schema/object-types schema-map)
                            :let [bad (schema/ambiguous-sides schema-map rid)]
                            :when (seq bad)]
                      (refusal/refuse! :refusal.platform-sdk/ambiguous-link-side
                        {:object-type (:object-type-name spec) :sides bad}
                        {:side (first bad)}))
        fingerprint (schema/fingerprint schema-map)
        types       (schema/object-type-index schema-map backing)
        rids        (schema/rid-index schema-map)
        actions     (schema/action-types schema-map)
        edit-names  (into #{} (keep schema/function-name)
                          (vals (or (:functions schema-map) {})))
        fns         (filter schema/function-name (schema/functions schema-map))
        edit?       #(contains? edit-names (schema/function-name %))
        ->file      #(ns-file % target)
        root-source (if (= :cljs target) cljs-ontology-source ontology-source)
        files       {(->file ns-sym)     (root-source ns-sym {:ontology ontology
                                                              :fingerprint fingerprint
                                                              :types types :rids rids})
                     (->file objects-ns) (objects-source objects-ns ns-sym types)
                     (->file actions-ns) (actions-source actions-ns actions)
                     (->file fns-ns)     (functions-source fns-ns fns edit?)}]
    {:ns ns-sym
     :ontology ontology
     :fingerprint fingerprint
     :namespaces {:ontology ns-sym :objects objects-ns :actions actions-ns :functions fns-ns}
     :files (into (sorted-map) (for [[path src] files] [path (read-all! path (str src "\n"))]))}))
