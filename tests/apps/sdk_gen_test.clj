(ns ^:unit apps.sdk-gen-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [platform-sdk.aggregate :as agg]
            [platform-sdk.client :as client]
            [platform-sdk.errors :as err]
            [platform-sdk.gen :as gen]
            [platform-sdk.objectset :as os]
            [platform-sdk.package :as pkg]
            [platform-sdk.predicates :as p]
            [platform-sdk.schema :as schema]))

(declare schema-map)

(deftest the-cljs-target-writes-cljs-and-claims-no-verify
  (let [{:keys [files]} (gen/generate schema-map {:ns 'my.sdk :target :cljs})]
    (is (every? #(str/ends-with? % ".cljs") (keys files)))
    (is (contains? files "my/sdk/actions.cljs"))
    (is (not (str/includes? (get files "my/sdk.cljs") "verify"))
        "a scoped token may not read the schema verify would compare with")))

(def schema-map
  {:object-types
   {"ri.sutra.main.object-types.aaa"
    {:object-type-name :file
     :attribute-list [{:attribute-name :name :attribute-type :string}
                      {:attribute-name :kind :attribute-type :keyword}
                      {:attribute-name :hash :attribute-type :string}
                      {:attribute-name :size :attribute-type :number}]
     :match-keys [:hash]
     :symbol "📄"
     :plural "Files"}
    "ri.sutra.main.object-types.bbb"
    {:object-type-name :support-case
     :attribute-list [{:attribute-name :name :attribute-type :string}
                      {:attribute-name :status :attribute-type :keyword}]
     :plural "Support cases"}
    "ri.sutra.main.object-types.ccc"
    {:object-type-name :person
     :attribute-list [{:attribute-name :name :attribute-type :string}]
     :plural "Personen"}}
   :actions
   {:create-file {:name :create-file
                  :description "Create a file object."
                  :params {:kind {:type :keyword}
                           :name {:type :string :required true}
                           :size {:type :long}
                           :tags {:type :string :multiple true}}}
    :resolve-case {:name :resolve-case
                   :params {:case {:type :support-case :required true}}}}
   :functions
   {"ri.sutra.main.functions.f1"
    {:name :schedule-maintenance :language :clojure :source "(fn [_] nil)"
     :param-types {:aircraft :string :hours :long}}
    "ri.sutra.main.functions.f2"
    {:name :untyped-fn :language :clojure :source "(fn [_] nil)"}}
   :link-types
   {"ri.sutra.main.link-types.e1"
    {:link-type-name :evidence-for
     :from-object-type :file
     :to-object-type :support-case
     :cardinality :many-to-many}
    "ri.sutra.main.link-types.e2"
    {:link-type-name :case-customer
     :from-object-type :support-case
     :to-object-type :person
     :cardinality :many-to-one}}})

(def self-link-schema
  {:object-types
   {"ri.sutra.main.object-types.ppp"
    {:object-type-name :projekt
     :attribute-list [{:attribute-name :name :attribute-type :string}]
     :match-keys [:name]
     :plural "Projekte"}}
   :link-types
   {"ri.sutra.main.link-types.hp"
    {:link-type-name :hauptprojekt-projekte
     :from-object-type "ri.sutra.main.object-types.ppp"
     :to-object-type "ri.sutra.main.object-types.ppp"
     :cardinality :many-to-one
     :forward-api-name :hauptprojekt
     :reverse-api-name :projekte}}})

(defn- sources [m opts] (:files (gen/generate m opts)))

(defn- source-of [m opts suffix]
  (some (fn [[path src]] (when (str/ends-with? path suffix) src)) (sources m opts)))

(defn- load-generated!
  [generated]
  (let [{:keys [namespaces files]} generated
        order [:ontology :objects :actions :functions]
        by-ns (into {} (for [[k ns-sym] namespaces]
                         [k (some (fn [[path src]]
                                    (when (= path (str (-> (str ns-sym)
                                                           (str/replace "-" "_")
                                                           (str/replace "." "/"))
                                                       ".clj"))
                                      src))
                                  files)]))]
    (doseq [k order] (load-string (get by-ns k)))
    namespaces))

(defn- unload! [namespaces] (doseq [[_ ns-sym] namespaces] (remove-ns ns-sym)))

(deftest fingerprint-agrees-with-the-python-platform-sdk
  (testing "the same ontology fingerprints identically in both generators"
    (is (= "087bde78ba579cb8" (schema/fingerprint schema-map)))))

(deftest fingerprint-is-shape-sensitive-not-order-sensitive
  (is (= (schema/fingerprint schema-map)
         (schema/fingerprint (update schema-map :object-types
                                     #(into {} (reverse (seq %)))))))
  (is (not= (schema/fingerprint schema-map)
            (schema/fingerprint (assoc-in schema-map
                                          [:object-types "ri.sutra.main.object-types.aaa"
                                           :object-type-name]
                                          :datei))))
  (is (not= (schema/fingerprint schema-map)
            (schema/fingerprint (update schema-map :actions dissoc :create-file))))
  (testing "renaming a link SIDE renames an accessor, so it moves the fingerprint"
    (is (not= (schema/fingerprint self-link-schema)
              (schema/fingerprint (assoc-in self-link-schema
                                            [:link-types "ri.sutra.main.link-types.hp"
                                             :reverse-api-name]
                                            :unterprojekte))))))

(deftest a-self-link-has-two-sides-each-with-its-own-name-and-direction
  (is (= "70a66b6f9bde860c" (schema/fingerprint self-link-schema)))
  (let [sides (get-in (schema/object-type-index self-link-schema nil) [:projekt :links])]
    (is (= [:hauptprojekt :projekte] (keys sides)))
    (is (= {:link-rid "ri.sutra.main.link-types.hp" :link-type-name :hauptprojekt-projekte
            :far-type :projekt :cardinality :one}
           (:hauptprojekt sides)))
    (is (= :many (:cardinality (:projekte sides)))))
  (let [namespaces (load-generated! (gen/generate self-link-schema {:ns 'selflink.sdk}))
        projekt    (resolve 'selflink.sdk.objects/projekt)]
    (try
      (testing "each side is its own hop, and the IR carries the side name — never a rid or a :dir"
        (let [rids {:types {:projekt "ri.sutra.main.object-types.ppp"}}]
          (is (= {:op :search-around :link :hauptprojekt
                  :in {:op :scan :root "ri.sutra.main.object-types.ppp"}}
                 (os/->ir (os/around (projekt) :hauptprojekt) rids)))
          (is (= {:op :search-around :link :projekte
                  :in {:op :scan :root "ri.sutra.main.object-types.ppp"}}
                 (os/->ir (os/around (projekt) :projekte) rids)))))
      (testing "the link-type name is not a side of a self-link"
        (is (thrown-with-msg? Exception #"is not a link of :projekt"
                              (os/around (projekt) :hauptprojekt-projekte))))
      (finally (unload! namespaces)))))

(deftest a-self-link-without-side-names-is-refused-at-generation
  (is (thrown-with-msg?
       Exception #"ambiguous on :projekt"
       (gen/generate (update-in self-link-schema [:link-types "ri.sutra.main.link-types.hp"]
                                dissoc :forward-api-name :reverse-api-name)
                     {:ns 'ambiguous.sdk}))))

(deftest a-hop-is-a-side-name-never-a-direction
  (is (thrown-with-msg? Exception #"names a link side"
                        (os/around (os/of :file) {:link :evidence-for :dir :from})))
  (is (thrown-with-msg? Exception #"names a link side"
                        (os/validate (assoc (os/of :file)
                                            :around [{:link :evidence-for :dir :to}])))))

(deftest token-spells-a-parametric-type-as-its-base
  (is (= "decimal" (schema/token [:decimal 6 3])))
  (is (= "" (schema/token nil)))
  (is (= "true" (schema/token true)))
  (is (= "false" (schema/token false))))

(deftest the-sdk-is-one-namespace-per-kind
  (let [{:keys [files namespaces
                fingerprint]} (gen/generate schema-map {:ns 'test.sdk :ontology "c10"})]
    (is (= #{"test/sdk.clj" "test/sdk/objects.clj" "test/sdk/actions.clj" "test/sdk/functions.clj"}
           (set (keys files))))
    (is (= {:ontology 'test.sdk :objects 'test.sdk.objects
            :actions 'test.sdk.actions :functions 'test.sdk.functions}
           namespaces))
    (is (= "087bde78ba579cb8" fingerprint))
    (is (str/includes? (get files "test/sdk.clj")
                       "(def ontology-fingerprint \"087bde78ba579cb8\")"))
    (is (str/includes? (get files "test/sdk/objects.clj")
                       "([client] (os/of client :file ontology/index))"))
    (is (str/includes? (get files "test/sdk/actions.clj") "(defn create-file!"))
    (is (str/includes? (get files "test/sdk/functions.clj") "(defn schedule-maintenance!"))
    (testing "every file reads as Clojure — the generator refuses to emit one that does not"
      (doseq [[path src] files]
        (is (seq (read-string (str "[" src "]"))) path)))))

(deftest a-name-that-collides-no-longer-overwrites-anything
  (let [colliding (-> schema-map
                      (assoc-in [:object-types "ri.sutra.main.object-types.aaa" :object-type-name]
                                :verify)
                      (assoc :actions {:verify {:name :verify :params {:x {:type :string}}}})
                      (assoc :functions {"f" {:function-name :verify :param-types {:y :string}}}))
        generated (gen/generate colliding {:ns 'collide.sdk})
        namespaces (load-generated! generated)]
    (try
      (is (fn? @(resolve 'collide.sdk.objects/verify)))
      (is (fn? @(resolve 'collide.sdk.actions/verify!)))
      (is (fn? @(resolve 'collide.sdk.functions/verify!)))
      (is (fn? @(resolve 'collide.sdk/verify)))
      (testing "the object-set constructor still answers with the type's own set"
        (is (= {:find :verify} (@(resolve 'collide.sdk.objects/verify)))))
      (finally (unload! namespaces)))))

(deftest a-generated-name-that-shadows-clojure-core-is-excluded-not-broken
  (let [shadowing (assoc-in schema-map
                            [:object-types "ri.sutra.main.object-types.ccc" :object-type-name]
                            :count)
        src (source-of shadowing {:ns 'shadow.sdk} "objects.clj")]
    (is (str/includes? src "(:refer-clojure :exclude [count])"))
    (let [namespaces (load-generated! (gen/generate shadowing {:ns 'shadow.sdk}))]
      (try (is (= {:find :count} (@(resolve 'shadow.sdk.objects/count))))
           (finally (unload! namespaces))))))

(deftest a-name-that-is-not-a-clojure-name-is-refused-at-generation
  (is (thrown-with-msg?
       Exception #"cannot spell object-type"
       (gen/generate (assoc-in schema-map
                               [:object-types "ri.sutra.main.object-types.aaa" :object-type-name]
                               (keyword "1/2 file"))
                     {:ns 'bad.sdk}))))

(deftest the-generated-action-declares-its-parameter-list
  (let [src (source-of schema-map {:ns 'test.sdk} "actions.clj")]
    (is (str/includes? src "{:required [:name] :params [:name :kind :size :tags]}"))
    (is (str/includes? src "{:required [:case] :params [:case]}"))
    (is (str/includes? src "Create a file object."))))

(deftest the-primary-key-comes-from-the-backing-map-not-from-the-first-attribute
  (let [live {:object-types
              {"ri.ot.1" {:object-type-name :arbeitsauftrag
                          :match-keys []
                          :attribute-list [{:attribute-name :ausfuehrender :attribute-type :string}
                                           {:attribute-name :auftrag-nr :attribute-type :string}]
                          :plural "Arbeitsaufträge"}}}
        backing {:object-types {"arbeitsauftrag" {:primary-key "auftrag-nr" :plural "Objekte"}}}
        types (schema/object-type-index live backing)]
    (is (= :auftrag-nr (get-in types [:arbeitsauftrag :primary-key])))))

(deftest functions-come-from-both-homes
  (let [live {:object-types {}
              :functions  {"ri.fn.1" {:function-name :freigabe-bestaetigen :language :clojure}}
              :transforms {"ri.tr.1" {:function-name :auftrag-karten
                                      :inputs {:auftrag-nr {:kind :scalar}
                                               :karten {:kind :object-type
                                                            :object-type "ri.ot.1"}}}
                           "ri.tr.2" {:transform-name :ein-gewoehnlicher-transform}}}
        src (source-of live {:ns 'probe.sdk} "functions.clj")]
    (is (= [:auftrag-karten :freigabe-bestaetigen]
           (sort (map schema/function-name (schema/functions live)))))
    (testing "an edit function mutates and says so; a query function does not"
      (is (str/includes? src "(defn freigabe-bestaetigen!"))
      (is (str/includes? src "(defn auftrag-karten\n")))
    (is (= [[:auftrag-nr :scalar] [:karten :object-type]]
           (mapv vec (schema/function-params (get-in live [:transforms "ri.tr.1"])))))))

(deftest the-package-is-a-project-a-separate-app-can-depend-on
  (let [dir (io/file (System/getProperty "java.io.tmpdir")
                     (str "sdk-pkg-" (System/currentTimeMillis)))
        _res (pkg/write! schema-map
                        {:ns 'linienwartung.sdk :ontology "linienwartung" :package? true
                         :runtime {:root "/opt/sutra/core"}}
                        dir)]
    (try
      (is (.isFile (io/file dir "deps.edn")))
      (is (.isFile (io/file dir "src/linienwartung/sdk/objects.clj")))
      (let [deps (read-string (slurp (io/file dir "deps.edn")))
            manifest (read-string (slurp (io/file dir "sdk.edn")))]
        (is (= ["src"] (:paths deps)))
        (is (= {:local/root "/opt/sutra/core"} (get-in deps [:deps 'sutra/core])))
        (is (= "0.1.0+087bde78ba579cb8" (:version manifest)))
        (is (= 'linienwartung.sdk (:ns manifest))))
      (testing "and what it stamps is what `sdk verify` reads back"
        (is (= (schema/fingerprint schema-map) (pkg/stamped-fingerprint (.getPath dir))))
        (is (= (schema/fingerprint schema-map)
               (pkg/stamped-fingerprint (.getPath (io/file dir "src/linienwartung/sdk.clj"))))))
      (finally (doseq [f (reverse (file-seq dir))] (.delete f))))))

(deftest the-package-refuses-a-project-with-no-runtime-to-depend-on
  (is (thrown-with-msg? Exception #"runtime coordinate"
                        (pkg/deps-edn {})))
  (is (= {:git/url "https://git" :git/sha "abc" :deps/root "libs/core"}
         (get (pkg/runtime-coordinate {:git "https://git" :sha "abc"}) 'sutra/core))))

(deftest object-set-builder-is-the-query-value
  (is (= {:find :file :where {:kind {:= "trace"}} :order-by :size :sort-dir :desc
          :limit 10 :offset 20}
         (-> (os/of :file)
             (os/where {:kind {:= "trace"}})
             (os/order :size :desc)
             (os/page 10 20)))))

(deftest predicates-compile-to-the-conjunctive-where
  (is (= {:size {:> 5 :<= 10} :name {:starts-with "a"}}
         (p/all (p/> :size 5) (p/<= :size 10) (p/starts-with :name "a"))))
  (is (= {:kind {:nil? true}} (p/nil? :kind)))
  (is (= {:evidence-for {:status {:= "open"}}}
         (p/linked :evidence-for (p/= :status "open"))))
  (is (thrown-with-msg? Exception #"names an attribute" (p/= "name" 1)))
  (is (= :usage (try (p/linked "evidence-for") (catch Exception e (err/kind e))))))

(deftest the-builder-knows-the-type-it-was-generated-for
  (let [namespaces (load-generated! (gen/generate schema-map {:ns 'typed.sdk}))
        file-set   (resolve 'typed.sdk.objects/file)]
    (try
      (is (= {:find :file :where {:kind {:= "trace"}}}
             (os/where (file-set) (p/= :kind "trace"))))
      (testing "an attribute the type does not have fails here, not at the endpoint"
        (is (thrown-with-msg? Exception #":nope is not an attribute or link of :file"
                              (os/where (file-set) (p/= :nope 1)))))
      (testing "a hop retypes the set, so the next filter is judged against the far side"
        (let [hopped (os/around (file-set) :evidence-for)]
          (is (= {:find :file :around [:evidence-for] :where {:status {:= "open"}}}
                 (os/where hopped (p/= :status "open"))))
          (is (thrown-with-msg? Exception #"not an attribute or link of :support-case"
                                (os/where hopped (p/= :hash "x"))))))
      (testing "and a link the type does not have is refused by name"
        (is (thrown-with-msg? Exception #":nope is not a link of :file"
                              (os/around (file-set) :nope))))
      (finally (unload! namespaces)))))

(deftest builder-refuses-what-the-server-refuses
  (is (thrown-with-msg? Exception #"offset needs a limit"
                        (os/validate (os/offset (os/of :file) 10))))
  (is (thrown-with-msg? Exception #"limit out of range"
                        (os/validate (os/limit (os/of :file) 20000))))
  (is (thrown-with-msg? Exception #"at most 3 hops"
                        (-> (os/of :file)
                            (os/around :evidence-for)
                            (os/around :case-customer)
                            (os/around :evidence-for)
                            (os/around :evidence-for))))
  (is (thrown-with-msg? Exception #"not a surface operator"
                        (os/where (os/of :file) {:kind {:like "x"}}))))

(deftest aggregates-are-judged-by-the-platform-grammar-before-they-are-sent
  (is (= {:find :file :aggs [{:fn :sum :attr :size :name :bytes}]
          :group-by [[:bucket :hash :month]]}
         (os/aggregate (os/of :file)
                       [(agg/as (agg/sum :size) :bytes)]
                       [(agg/bucket :hash :month)])))
  (is (= [:kind :size-sum] (agg/result-keys [(agg/sum :size)] [(agg/by :kind)])))
  (is (thrown-with-msg? Exception #"needs an attribute"
                        (os/aggregate (os/of :file) [{:fn :sum}])))
  (is (thrown-with-msg? Exception #"unknown aggregate function|unknown"
                        (os/aggregate (os/of :file) [{:fn :median :attr :size}]))))

(deftest set-algebra-lowers-to-object-set-ir
  (let [rids {:types {:file "ri.sutra.main.object-types.aaa"}}
        traces (os/where (os/of :file) {:kind {:= "trace"}})
        big (os/where (os/of :file) {:size {:> 200}})]
    (is (= {:op :union
            :in [{:op :filter
                  :where [:and [:= :kind "trace"]]
                  :in {:op :scan :root "ri.sutra.main.object-types.aaa"}}
                 {:op :filter
                  :where [:and [:> :size 200]]
                  :in {:op :scan :root "ri.sutra.main.object-types.aaa"}}]}
           (os/combine :union traces big rids)))
    (is (= {:op :search-around
            :link :evidence-for
            :in {:op :scan :root "ri.sutra.main.object-types.aaa"}}
           (os/->ir (os/around (os/of :file) :evidence-for) rids)))
    (is (thrown-with-msg? Exception #"paged set cannot be a member"
                          (os/->ir (os/limit (os/of :file) 5) rids)))
    (is (thrown-with-msg? Exception #"no rid for object type"
                          (os/->ir (os/of :unknown) rids)))))

(deftest the-client-is-a-value-and-says-so-when-it-is-not
  (is (thrown-with-msg? Exception #":base-url is required" (client/client {})))
  (let [c (client/client {:base-url "https://example.test/" :token "t"})]
    (is (= "https://example.test" (:base-url c)))
    (is (= "t" (client/token c)))
    (is (client/client? c))
    (is (not (client/client? {:base-url "x"}))))
  (is (= :usage (try (client/request {:not :a-client} :get :schema)
                     (catch Exception e (err/kind e))))))
