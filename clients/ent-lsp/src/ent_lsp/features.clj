(ns ent-lsp.features
  (:require [clojure.string :as str]
            [ent-lsp.analysis :as an]
            [ent-lsp.cursor :as cursor]
            [ent-lsp.grammar :as grammar]
            [ent-lsp.spec :as spec]
            [ent-lsp.read :as read]
            [ent-lsp.sutra :as sutra]
            [sutra.parser.authoring :as authoring]))

(defn lsp-range [{:keys [row col end-row end-col]}]
  {:start {:line (dec row) :character (dec col)}
   :end   {:line (dec (or end-row row)) :character (dec (or end-col (inc col)))}})

(defn- span [a b]
  (let [before? (fn [x y]
                  (or (< (:row x) (:row y)) (and (= (:row x) (:row y)) (< (:col x) (:col y)))))
        start   (if (before? a b) a b)
        end     (if (before? {:row (:end-row a) :col (:end-col a)}
                             {:row (:end-row b) :col (:end-col b)}) b a)]
    {:row (:row start) :col (:col start) :end-row (:end-row end) :end-col (:end-col end)}))

(def severity {:error 1 :warning 2 :information 3 :hint 4})

(defn diagnostic [{:keys [loc severity rule message vars]}]
  (cond-> {:range    (lsp-range loc)
           :severity (get ent-lsp.features/severity severity 1)
           :source   "ent"
           :code     (name rule)
           :message  message}
    (:data vars) (assoc :data (assoc (:data vars) :rule (name rule)))
    (= :veraltet rule) (assoc :tags [2])))

(defn symbol-at
  [ws rel line character]
  (when-let [parsed (get-in ws [:files rel :parsed])]
    (when-let [leaf (read/leaf-at parsed (inc line) (inc character))]
      (let [idx  (:index ws)
            loc  (:loc leaf)
            d    (some #(when (= loc (:name-loc %)) %) (get-in idx [:defs-by-file rel]))
            r    (some #(when (= loc (:loc %)) %) (get-in idx [:refs-by-file rel]))]
        {:leaf leaf :def d :ref r
         :targets (cond r (:targets r)
                        d [d]
                        (:key? leaf) []
                        :else (get-in idx [:by-name (:value leaf)]))}))))

(defn- describe [d]
  (let [v (:data d)
        v (if (vector? v) (get v 4) v)]
    (when (map? v)
      (or (:description v) (:display-name v) (when (string? (:name v)) (:name v))))))

(defn- owner-line [d]
  (if-let [o (:owner d)]
    (str "Projekt `" (an/show o) "`"
         (case (:owner-source d)
           :declared ""
           :derived  (str " — abgeleitet über `" (an/show (:via d)) "`")
           ""))
    "ohne Besitzer"))

(defn- ref-count [ws d]
  (count (filter (fn [r] (some #(an/same-symbol? % d) (:targets r)))
                 (get-in ws [:index :refs]))))

(defn hover [ws rel line character]
  (when-let [{:keys [leaf ref targets]} (symbol-at ws rel line character)]
    (let [body (cond
                 (seq targets)
                 (str/join "\n\n---\n\n"
                           (for [d (take 3 targets)]
                             (str "**" (name (:kind d)) "** `" (an/show (:name d)) "`"
                                  (when-let [o (:object d)]
                                    (str " von `" (an/show o) "`"
                                         (when-let [t (get-in d [:data :type])]
                                           (str " · Typ `" (an/show t) "`"))
                                         (when (some #{(:name d)}
                                                     (an/primary-key (:spec ws) (:index ws) o))
                                           " · Primärschlüssel")))
                                  " · " (owner-line d)
                                  (when-not (:object d)
                                    (str " · Status `" (an/show (an/status-of (:spec ws) d)) "`"))
                                  (when (= :link (:kind d))
                                    (let [[_ from to card] (:data d)]
                                      (str "\n\n`" (an/show from) "` → `" (an/show to) "` · "
                                           (an/show card))))
                                  "\n\n" (:file d) ":" (get-in d [:name-loc :row])
                                  " · " (ref-count ws d) " Verweise"
                                  (when-let [t (describe d)]
                                    (str "\n\n"
                                         (if (> (count t) 700) (str (subs t 0 700) " …") t))))))
                 (:object ref) (str "`" (an/show (:object ref)) "` hat keine Eigenschaft `"
                                    (an/show (:name ref)) "`")
                 ref (str "`" (an/show (:name ref)) "` ist nirgends definiert ("
                          (str/join "/" (map name (:kinds ref))) ")"))]
      (when body
        {:contents {:kind "markdown" :value body}
         :range    (lsp-range (:loc leaf))}))))

(defn location [uri-of d]
  {:uri (uri-of (:file d)) :range (lsp-range (:name-loc d))})

(defn definition [ws uri-of rel line character]
  (mapv #(location uri-of %) (:targets (symbol-at ws rel line character))))

(defn references [ws uri-of rel line character include-declaration?]
  (when-let [{:keys [targets]} (symbol-at ws rel line character)]
    (let [keys (set (map (juxt :kind :name :object) targets))
          refs (filter (fn [r] (some #(keys ((juxt :kind :name :object) %)) (:targets r)))
                       (get-in ws [:index :refs]))]
      (vec (concat (when include-declaration? (map #(location uri-of %) targets))
                   (map (fn [r] {:uri (uri-of (:file r)) :range (lsp-range (:loc r))}) refs))))))

(def ^:private completion-kind
  {:property 10 :object 7 :interface 8 :link 18 :action 3 :dataset 22 :source 9 :transform 2
   :function 3
   :project 19 :value-type 25 :shared-property 10 :writeback 23 :application 9})

(def ^:private property-slots #{:title :set :attribute :primary-key})

(defn- slot-kinds
  [spec text-before]
  (when-let [[_ k] (re-find #":([^\s\[\]{}(),\"]+)\s+\[?[^\s\[\]{}(),\"]*$" text-before)]
    (if (property-slots (keyword k))
      #{:property}
      (some (fn [r] (when (= (keyword k) (:key r)) (if (set? (:kind r)) (:kind r) #{(:kind r)})))
            (:references spec)))))

(defn- scope-objects
  [ws rel text-before]
  (if-let [[_ o] (last (re-seq #":object-type\s+:([^\s\[\]{}(),\"]+)" text-before))]
    #{(keyword o)}
    (set (keep #(when (= :object (:kind %)) (:name %)) (get-in ws [:index :defs-by-file rel])))))

(defn- grammar-items
  [ws rel here line character]
  (let [kind  (get-in ws [:files rel :kind])
        start (- character (count (:prefix here)))
        item  (fn [label kind detail sort]
                {:label label :kind kind :detail detail :sortText (str sort label) :filterText label
                 :textEdit {:range {:start {:line line :character start}
                                    :end {:line line :character character}}
                            :newText label}})
        path  (case (:at here)
                :key     (:path here)
                :value   (conj (:path here) (:key here))
                :element (conj (:path here) (:index here)))]
    (if (= :key (:at here))
      (for [{:keys [key required? type]} (grammar/keys-at kind path)
            :when (not (contains? (:keys here) key))]
        (item (str key) 14 (str (when required? "Pflicht · ") type) (if required? "00" "01")))
      (concat
       (for [v (grammar/values-at kind path)]
         (item (pr-str v) 20 "Wert laut Grammatik" "00"))
       (when (spec/match? [:schema :actions :* :rules :** :op] path)
         (for [op (get-in ws [:spec :actions :rule-ops])]
           (item (str op) 20 "Regel-Operation (surface.ebnf)" "00")))))))

(defn completion [ws rel line character]
  (let [text   (get-in ws [:files rel :text] "")
        lines  (str/split-lines text)
        before (subs (get lines line "") 0 (min character (count (get lines line ""))))
        here   (cursor/locate (str (str/join "\n" (take line lines)) (when (pos? line) "\n")
                                   before))
        gitems (when here (grammar-items ws rel here line character))
        prefix (or (re-find #":[^\s\[\]{}(),\"';]*$" before) "")
        start  (- character (count prefix))
        want   (slot-kinds (:spec ws) (subs before 0 start))
        scope  (when (contains? want :property)
                 (scope-objects ws rel (str (str/join "\n" (take line lines)) "\n" before)))
        defs   (->> (get-in ws [:index :defs])
                    (filter #(keyword? (:name %)))
                    (filter #(if (:object %) (contains? scope (:object %)) true))
                    (map (fn [d] [((juxt :kind :name :object) d) d]))
                    (into {})
                    vals)]
    {:isIncomplete false
     :items (if (or (= :key (:at here)) (and (seq gitems) (not want)))
              (vec gitems)
              (into (vec gitems)
                    (for [d defs
                       :let [label (str (:name d))]]
                   {:label    label
                    :kind     (completion-kind (:kind d) 6)
                    :detail   (if-let [o (:object d)]
                                (str "property · " (an/show o)
                                     (when-let [t (get-in d [:data :type])]
                                       (str " · " (an/show t))))
                                (str (name (:kind d))
                                     (when-let [o (:owner d)] (str " · " (an/show o)))))
                    :sortText (str (if (and want (contains? want (:kind d))) "0" "1") label)
                    :filterText label
                    :textEdit {:range {:start {:line line :character start}
                                       :end   {:line line :character character}}
                               :newText label}})))}))

(def ^:private symbol-kind
  {:property 7 :object 5 :interface 11 :link 20 :action 12 :dataset 23 :source 2 :transform 6
   :function 12
   :project 4 :value-type 26 :shared-property 7 :writeback 24 :application 2 :object-view 2
   :repository 3 :pipeline 6 :collection 3 :workspace 3 :custom-application 2 :object-set 18})

(defn- prefix? [p q] (and (< (count p) (count q)) (= p (subvec q 0 (count p)))))

(defn document-symbols [ws rel]
  (let [defs   (sort-by (juxt (comp count :path) (comp :row :name-loc))
                        (get-in ws [:index :defs-by-file rel]))
        parsed (get-in ws [:files rel :parsed])
        sym    (fn [d] {:name (an/show (:name d))
                        :detail (str (name (:kind d))
                                     (when-let [o (:owner d)] (str " · " (an/show o))))
                        :kind (symbol-kind (:kind d) 13)
                        :range (lsp-range (span (:name-loc d) (:loc d)))
                        :selectionRange (lsp-range (:name-loc d))
                        :path (:path d)})
        nest   (fn nest [parent-path ds]
                 (let [direct (filter (fn [d] (and (prefix? parent-path (:path d))
                                                   (not (some #(and (prefix? parent-path (:path %))
                                                                    (prefix? (:path %) (:path d)))
                                                              ds))))
                                      ds)]
                   (mapv (fn [d] (let [kids (nest (:path d) ds)]
                                   (cond-> (dissoc (sym d) :path)
                                     (seq kids) (assoc :children kids))))
                         direct)))]
    (if (and (str/ends-with? rel ".ent") (:by-path parsed))
      (vec (for [[section ds] (group-by #(get (:path %) 1) defs)
                 :when section
                 :let [kn (get-in parsed [:by-path [:key [:schema section]]])
                       vn (get-in parsed [:by-path [:schema section]])]
                 :when (and kn vn)]
             {:name (str section) :kind 3
              :detail (str (count ds))
              :range (lsp-range (span (:loc kn) (:loc vn)))
              :selectionRange (lsp-range (:loc kn))
              :children (nest [:schema section] ds)}))
      (nest [] defs))))

(defn workspace-symbols [ws uri-of query]
  (let [q (str/lower-case (or query ""))]
    (->> (get-in ws [:index :defs])
         (filter #(str/includes? (str/lower-case (str (:name %))) q))
         (take 300)
         (mapv (fn [d] {:name (an/show (:name d))
                        :kind (symbol-kind (:kind d) 13)
                        :containerName (str (name (:kind d))
                                            (when-let [o (:owner d)] (str " · " (an/show o))))
                        :location (location uri-of d)})))))

(declare before-close)

(defn- declare-with-import
  [ws site owner file]
  (let [parsed (get-in ws [:files file :parsed])]
    (when (and file (not (:synthetic parsed)) (= :map (:coll (get-in parsed [:by-path [:schema]]))))
      (when-let [pos (before-close parsed [:schema])]
        {:file file
         :edit {:range {:start pos :end pos}
                :newText (str (when (seq (get-in parsed [:data :schema])) "\n ")
                              ":projects {:" (name site) " {:imports [" owner "]}}")}}))))

(defn- import-edit
  [ws site owner & [file]]
  (let [cands (get-in ws [:index :by-key [:project site]])
        d     (or (some #(when (contains? (:data %) :imports) %) cands)
                  (some #(when (= (:file %) (str (name site) "/project.ent")) %) cands)
                  (first cands))]
    (if-not d
      (declare-with-import ws site owner file)
      (let [parsed (get-in ws [:files (:file d) :parsed])
            imps   (get-in parsed [:by-path (conj (:path d) :imports)])
            node   (get-in parsed [:by-path (:path d)])
            [loc text] (if (= :vector (:coll imps))
                         [(:loc imps)
                          (str (when (seq (get-in parsed (into [:data] (conj (:path d) :imports))))
                                 " ") owner)]
                         [(:loc node) (str " :imports [" owner "]")])
            pos    {:line (dec (:end-row loc)) :character (- (:end-col loc) 2)}]
        {:file (:file d) :edit {:range {:start pos :end pos} :newText text}}))))

(defn- import-action [ws uri-of dg {:keys [site owner]}]
  (let [s (keyword site) o (keyword owner)]
    (when-not (an/import-cycle? (:index ws) s o)
      (when-let [{:keys [file edit]} (import-edit ws s o)]
        {:title       (str "Import " o " in " s " ergänzen")
         :kind        "quickfix"
         :diagnostics [dg]
         :isPreferred true
         :edit        {:changes {(uri-of file) [edit]}}}))))

(defn- move-action
  [ws uri-of rel dg {:keys [expected]}]
  (let [own (remove :object (get-in ws [:index :defs-by-file rel]))]
    (when (and expected (= 1 (count own)) (not (contains? (:files ws) expected)))
      {:title       (str "Nach " expected " verschieben")
       :kind        "quickfix"
       :diagnostics [dg]
       :edit        {:documentChanges [{:kind "rename" :oldUri (uri-of rel)
                                        :newUri (uri-of expected)
                                        :options {:overwrite false :ignoreIfExists false}}]}})))

(defn- project-action
  [ws uri-of rel dg {:keys [project]}]
  (let [{:keys [line character]} (get-in dg [:range :start])
        d      (some #(when (and (= (inc line) (get-in % [:name-loc :row]))
                                 (= (inc character) (get-in % [:name-loc :col])))
                        %)
                     (get-in ws [:index :defs-by-file rel]))
        node   (when (and d (map? (:data d))) (get-in ws [:files rel :parsed :by-path (:path d)]))
        {:keys [row col]} (:loc node)]
    (when (and project (= :map (:coll node)))
      {:title       (str ":project :" (name (keyword project)) " ergänzen")
       :kind        "quickfix"
       :diagnostics [dg]
       :isPreferred true
       :edit        {:changes {(uri-of rel) [{:range {:start {:line (dec row) :character col}
                                                      :end   {:line (dec row) :character col}}
                                              :newText
                                                (str ":project :" (name (keyword project))
                                                     " ")}]}}})))

(defn- replace-action [uri-of rel dg {:keys [replacement]}]
  (when replacement
    (let [r (str ":" (name (keyword replacement)))]
      {:title       (str "Durch " r " ersetzen")
       :kind        "quickfix"
       :diagnostics [dg]
       :isPreferred true
       :edit        {:changes {(uri-of rel) [{:range (:range dg) :newText r}]}}})))

(defn skeleton-text [project nm]
  (let [n (name (keyword nm)) p (name (keyword project))]
    (str "{:schema\n {:objects\n  {:" n " {:project    :" p "\n"
         (apply str (repeat (+ 5 (count n)) " ")) ":title      :name\n"
         (apply str (repeat (+ 5 (count n)) " ")) ":attributes [[:" n
         "-id :string] [:name :string]]}}}}\n")))

(defn- before-close
  [parsed path]
  (when-let [{:keys [end-row end-col]} (:loc (get-in parsed [:by-path path]))]
    {:line (dec end-row) :character (- end-col 2)}))

(defn- projekttext-skeleton-action
  [ws uri-of dg {:keys [skeleton into]}]
  (let [parsed  (get-in ws [:files into :parsed])
        n       (name (keyword skeleton))
        entry   (str ":" n " {:title :name :attributes [[:" n "-id :string] [:name :string]]}")
        objects (get-in parsed [:by-path [:schema :objects]])
        [pos text] (if (= :map (:coll objects))
                     [(before-close parsed [:schema :objects])
                      (str (when (seq (get-in parsed [:data :schema :objects])) "\n   ") entry)]
                     [(before-close parsed [:schema]) (str "\n :objects {" entry "}")])]
    (when pos
      {:title       (str "Object-Type :" n
                         " hier anlegen — dieser Projekttext wird sein Autorenort")
       :kind        "quickfix"
       :diagnostics [dg]
       :isPreferred true
       :edit        {:changes {(uri-of into) [{:range {:start pos :end pos} :newText text}]}}})))

(defn- import-instead-action
  [ws uri-of dg {:keys [site owner file path]}]
  (let [s (keyword site) o (keyword owner)
        parsed (get-in ws [:files file :parsed])
        path   (vec path)
        val    (:loc (get-in parsed [:by-path path]))
        key    (:loc (get-in parsed [:by-path [:key path]]))
        imp    (when-not (an/import-cycle? (:index ws) s o)
                 (when-not (contains? (an/reachable (:spec ws) (:index ws) s) o)
                   (import-edit ws s o file)))]
    (when (and val (not (an/import-cycle? (:index ws) s o)))
      {:title       (str "Importieren statt deklarieren: " (name s) " importiert " (name o))
       :kind        "quickfix"
       :diagnostics [dg]
       :isPreferred true
       :edit        {:changes (merge-with into
                                          {(uri-of file)
                                             [{:range (lsp-range (if key (span key val) val))
                                               :newText ""}]}
                                          (when imp {(uri-of (:file imp)) [(:edit imp)]}))}})))

(defn- skeleton-action
  [ws uri-of dg {:keys [skeleton project into] :as data}]
  (if into
    (projekttext-skeleton-action ws uri-of dg data)
   (when (and skeleton project)
    (let [path (str (name (keyword project)) "/" (name (keyword skeleton)) "/object.ent")]
      (when-not (contains? (:files ws) path)
        {:title       (str "Object-Type :" (name (keyword skeleton)) " in " path " anlegen")
         :kind        "quickfix"
         :diagnostics [dg]
         :edit        {:documentChanges
                       [{:kind "create" :uri (uri-of path)
                         :options {:overwrite false :ignoreIfExists true}}
                        {:textDocument {:uri (uri-of path) :version nil}
                         :edits [{:range {:start {:line 0 :character 0} :end {:line 0 :character 0}}
                                  :newText (skeleton-text project skeleton)}]}]}})))))

(defn- synonym-edit
  [ws file alias target]
  (let [parsed (get-in ws [:files file :parsed])
        pname  (first (keys (get-in parsed [:data :schema :projects])))
        ppath  [:schema :projects pname]
        syn    (get-in parsed [:by-path (conj ppath :synonyms)])
        entry  (str ":" (name alias) " :" (name target))]
    (when pname
      (if (= :map (:coll syn))
        (let [pos (before-close parsed (conj ppath :synonyms))]
          {:range {:start pos :end pos}
           :newText (str (when (seq (get-in parsed (into [:data] (conj ppath :synonyms)))) " ")
                         entry)})
        (when-let [pos (before-close parsed ppath)]
          {:range {:start pos :end pos}
           :newText (str (when (seq (get-in parsed (into [:data] ppath))) " ") ":synonyms {" entry
                         "}")})))))

(defn- synonym-actions
  [ws uri-of dg {:keys [skeleton project] file :into}]
  (when-let [suggest (:suggest ws)]
    (let [site (keyword project)]
      (for [{:keys [name owner] :as c} (take 3 (suggest (keyword skeleton) :object))
            :let [target (keyword name)
                  o      (some-> owner keyword)
                  syn    (synonym-edit ws file (keyword skeleton) target)
                  imp    (when (and o (not= o site)
                                    (not (contains? (an/reachable (:spec ws) (:index ws) site) o))
                                    (not (an/import-cycle? (:index ws) site o)))
                           (import-edit ws site o))]
            :when syn]
        {:title       (str "Als Synonym übernehmen: :" (clojure.core/name (keyword skeleton)) " → "
                           (sutra/describe c))
         :kind        "quickfix"
         :diagnostics [dg]
         :edit        {:changes (merge-with into {(uri-of file) [syn]}
                                            (when imp {(uri-of (:file imp)) [(:edit imp)]}))}}))))

(defn projekt-template
  [ws rel]
  (let [fmts (:formats ws)
        fid  (authoring/default-format fmts rel)
        decl (get fmts fid)
        p    (authoring/implied-project fmts rel)]
    (when (and decl p (seq (:owner decl)))
      (let [by-name?  (some #{:file-name :facet} (:owner decl))
            declared? (some #(not= rel (:file %))
                            (get-in ws [:index :by-key [:project (keyword p)]]))
            secs      (or (seq (remove #{:projects} (:sections decl))) [:objects :links])
            body      (cond-> (into {} (for [sec secs] [sec (if (= :links sec) [] {})]))
                        (not (or by-name? declared?)) (assoc :projects {(keyword p) {}}))
            text      (authoring/print-file fmts fid body {})]
        (if (= :edn (:language decl))
          (str ";; Projekt " p (some->> (:description decl) (str " — ")) "\n"
               ";; Ein Typ, den es schon gibt, wird importiert, nicht neu deklariert: ent-lsp "
               "schlägt den Import vor.\n"
               text)
          text)))))

(defn- template-action [ws uri-of rel]
  (when (and rel (str/blank? (get-in ws [:files rel :text])))
    (when-let [t (projekt-template ws rel)]
      {:title "Projekt-Gerüst einfügen"
       :kind  "quickfix"
       :isPreferred true
       :edit
         {:changes {(uri-of rel) [{:range {:start {:line 0 :character 0}
                                           :end {:line 0 :character 0}}
                                   :newText t}]}}})))

(defn code-actions
  ([ws uri-of diagnostics] (code-actions ws uri-of nil diagnostics))
  ([ws uri-of rel diagnostics]
   (vec (concat
         (keep identity [(template-action ws uri-of rel)])
         (mapcat (fn [dg]
                (if (get-in ws [:files (get-in dg [:data :file] rel) :parsed :synthetic])
                  []
                (let [data (:data dg)]
                  (if (and (= "unbekannt" (:rule data)) (:into data))
                    (concat (synonym-actions ws uri-of dg data)
                            (keep identity [(skeleton-action ws uri-of dg data)]))
                  (keep identity
                   [(case (:rule data)
                    "import-fehlt" (import-action ws uri-of dg data)
                    "falscher-ort" (when rel (move-action ws uri-of rel dg data))
                    "ohne-besitzer" (when rel (project-action ws uri-of rel dg data))
                    "veraltet" (when rel (replace-action uri-of rel dg data))
                    "unbekannt" (skeleton-action ws uri-of dg data)
                    "projekttext-gibt-es-schon" (import-instead-action ws uri-of dg data)
                    nil)])))))
              diagnostics)))))

(defn- rename-target [ws rel line character]
  (when-let [{:keys [leaf targets]} (symbol-at ws rel line character)]
    (let [d (first targets)]
      (when (and d (keyword? (:name d)) (not= :project (:kind d)))
        {:leaf leaf :def d}))))

(defn- refuse-protected! [ws d]
  (let [spec (:spec ws)
        owner (if (:object d) (first (get-in ws [:index :by-key [:object (:object d)]])) d)
        st (an/status-of spec owner)]
    (when (contains? (get-in spec [:status :protected]) st)
      (throw (ex-info (str (an/show (:name d)) " gehört zu einer Ressource mit Status " (an/show st)
                           " — ihr Name bleibt; erst deprecated, dann neu anlegen")
                      {:code -32803})))))

(defn prepare-rename [ws rel line character]
  (when-let [{:keys [leaf def]} (rename-target ws rel line character)]
    (refuse-protected! ws def)
    {:range (lsp-range (:loc leaf)) :placeholder (name (:name def))}))

(defn rename [ws uri-of rel line character new-name]
  (when-let [{:keys [def]} (rename-target ws rel line character)]
    (refuse-protected! ws def)
    (let [nm (str/replace (str new-name) #"^:" "")]
      (when-not (re-find (re-pattern (get-in ws [:spec :identifiers :pattern])) nm)
        (throw (ex-info (str nm " ist keine gültige ID") {:code -32602})))
      (let [idx   (:index ws)
            defs  (filter #(an/same-symbol? % def) (:defs idx))
            refs  (filter (fn [r] (some #(an/same-symbol? % def) (:targets r))) (:refs idx))
            spots (distinct (concat (map (juxt :file :name-loc) defs)
                                    (map (juxt :file :loc) refs)))]
        {:changes (reduce (fn [m [file loc]]
                            (update m (uri-of file) (fnil conj [])
                                    {:range (lsp-range loc) :newText (str ":" nm)}))
                          {} spots)}))))
