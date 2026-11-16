(ns ^:unit platform.ontology-tree-test
  (:require [clojure.edn :as edn]
            [sutra.parser.authoring.standard :as standard]
            [clojure.set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.parser.project-export :as pe]
            [sutra.parser.project-emit :as pemit]))

(def ^:private schema
  {:projections    standard/projections
   :projects     {"ri.sutra.main.project.p1" {:project-name :controlling}}
   :repositories {"ri.sutra.main.repository.r1"
                  {:repository-name :etl :project "ri.sutra.main.project.p1"}}
   :object-types {"ri.sutra.main.object-type.o1"
                  {:object-type-name "widget"
                   :project          "ri.sutra.main.project.p1"
                   :attribute-list   [{:attribute-name "id" :attribute-type "string"}]
                   :primary-key      ["id"]
                   :plural "Widgets"}
                  "ri.sutra.main.object-type.o2"
                  {:object-type-name "gadget"
                   :attribute-list   [{:attribute-name "id" :attribute-type "string"}]
                   :primary-key      ["id"]
                   :plural "Gadgets"}}
   :transforms   {"ri.sutra.main.transform.t1"
                  {:transform-name "parse"
                   :language       :python
                   :code           "print(1)"
                   :project        "ri.sutra.main.project.p1"
                   :repository     "ri.sutra.main.repository.r1"}}})

(defn- ont-schema
  [s]
  (apply merge-with merge (for [[_ {:keys [content]}] (pe/ontology-tree s)]
                            (:schema (edn/read-string content)))))

(deftest the-tree-carries-types-from-every-project
  (testing "a type owned by `controlling` is part of the ontology exactly as much
            as an unassigned one. `export`'s default scope means UNASSIGNED, so
            inheriting it here would produce a checkout missing most of the
            instance — and a push of that checkout reads as a mass retirement."
    (is (= #{"widget" "gadget"} (set (keys (:objects (ont-schema schema))))))))

(deftest project-and-all-projects-are-different-scopes-and-saying-both-throws
  (testing "`:project nil` already means something specific and narrow, so
            'the whole ontology' needed a scope of its own rather than an
            overload of the absent one"
    (is (= #{"gadget"}
           (set (keys (:objects (:schema (:ent (pemit/export schema {}))))))))
    (is (thrown? Exception (pemit/export schema {:all-projects? true :project :controlling})))))

(deftest the-tree-carries-no-code
  (testing "transform bodies and app sources belong to the repositories that own
            them; emitting them here would put every transform in two trees"
    (is (= [pe/ontology-ent-path "controlling.objects.ent"]
           (sort-by count (keys (pe/ontology-tree schema)))))
    (let [txt (apply str (map :content (vals (pe/ontology-tree schema))))]
      (is (not (str/includes? txt "print(1)"))
          "`:inline? false` is what keeps :code out — not a filter that could be
           forgotten, but the same flag `repo-tree` passes for the same reason"))))

(deftest sources-and-datasets-are-projected-into-transform-ent
  (let [s    (assoc schema
                    :projections standard/projections
                    :datasets
                      {"ri.sutra.main.dataset.d1" {:dataset-name :widget_raw :kind :standalone}}
                    :sources  {"ri.sutra.main.source.s1" {:source-name :erp :type :jdbc}})
        tree (pe/ontology-tree s)
        read (fn [p] (:schema (edn/read-string (:content (get tree p)))))]
    (is (= #{pe/ontology-ent-path pe/transform-ent-path "controlling.objects.ent"}
           (set (keys tree))))
    (is (= #{:datasets :sources} (set (keys (read pe/transform-ent-path)))))
    (is (not-any? #{:sources :datasets} (keys (read pe/ontology-ent-path))))
    (is (contains? (read pe/ontology-ent-path) :projections)
        "the projections that say so live in ontology.ent")
    (is (contains? (read pe/ontology-ent-path) :objects))))

(deftest the-two-trees-own-disjoint-sections
  (is (empty? (clojure.set/intersection pe/code-sections pe/type-sections)))
  (testing "the containers are the deliberate exception — both trees emit them so
            their members' `:project` refs resolve, and neither can edit them out
            from under the other because both render the same rows"
    (is (every? pe/repo-sections pe/container-sections))
    (is (every? pe/ontology-sections pe/container-sections))))

(deftest each-tree-emits-only-what-it-owns
  (let [ont (ont-schema schema)
        rep (:schema (edn/read-string
                      (:content (get (pe/repo-tree schema :etl) "etl.ent"))))]
    (is (every? pe/ontology-sections (keys ont)))
    (is (every? pe/repo-sections (keys rep)))
    (testing "and the halves are actually populated — a tree that emitted nothing
              would satisfy 'only what it owns' vacuously"
      (is (contains? ont :objects))
      (is (contains? rep :transforms))
      (is (not (contains? ont :transforms)))
      (is (not (contains? rep :objects))))))

(deftest a-section-with-no-home-fails-at-the-export
  (testing "a new section added to `export` without a home would otherwise be
            silently absent from every checkout — and a push omitting it would
            then look like a deliberate deletion"
    (is (nil? (pe/partition-check! [:objects :transforms :projects] ::t)))
    (let [e (try (pe/partition-check! [:objects :sprockets] ::t) nil
                 (catch Exception e e))]
      (is (some? e))
      (is (str/includes? (ex-message e) ":sprockets") "the section is NAMED")
      (is (str/includes? (ex-message e) "type-sections")
          "and the message says where to put it"))))

(deftest the-tree-names-only-itself
  (testing "a repository in the schema used to become a submodule here — a
            gitlink plus a `.gitmodules` line. Both are gone: a pointer makes the
            ontology's history contain a code repository's VERSION, which is what
            per-resource versioning denies. The layout it produced is a checkout
            layout and `sutra clone ontology` still lays it down, as independent
            clones."
    (is (every? #(str/ends-with? % ".ent") (keys (pe/ontology-tree schema))))
    (is (nil? (get (pe/ontology-tree schema) "etl")))))

(deftest gitmodules-stays-ENTITY-plane-with-nothing-emitting-it
  (testing "which is what REMOVES it. An entity-plane path the exporter does not
            emit is one the next projection deletes, so every existing clone
            loses the stale file and a hand-written one is removed again. File
            plane would carry it forward forever, in a repository that has no
            submodules to declare."
    (is (= :entity (pe/plane-of {} ".gitmodules")))
    (is (not (contains? (pe/ontology-tree schema) ".gitmodules")))))

(deftest a-links-lifecycle-survives-the-round-trip
  (testing "an object type carries `:status`/`:deprecation` through the shared
            `sutra.types.shape` row; `emit-links` is hand-written and did not, so a
            live `:active` link exported WITHOUT its status. Two things went
            wrong with that. The retire gate reads status out of this export, so
            a protected link read as freely retirable; and install's plan-diff
            already compares the field both ways (`canonical-conn->wire-args`),
            so the absent side made every push stage an update for every statused
            link — a phantom change, forever."
    (let [s (assoc schema :link-types
                   {"ri.sutra.main.link-type.l1"
                    {:link-type-name    "owns"
                     :from-object-type  "widget"
                     :to-object-type    "gadget"
                     :cardinality       :many-to-one
                     :status            :active
                     :deprecation       {:replacement :holds}}})
          links (:links (ont-schema s))]
      (is (= :active (:status (get (first links) 4))))
      (is (= {:replacement :holds} (:deprecation (get (first links) 4))))
      (is (= {"owns" :active} (:links (pe/declared-type-statuses (ont-schema s))))
          "and the retire gate's reader sees it"))))

(deftest declared-type-names-reads-both-vocabularies
  (testing "an `.ent` writes names as keywords and a live schema stores them as
            strings; comparing those two directly yields a difference that is
            always non-empty and never real"
    (is (= {:objects #{"widget"}}
           (pe/declared-type-names {:objects {:widget {}}})))
    (is (= {:objects #{"widget"}}
           (pe/declared-type-names {:objects {"widget" {}}})))))

(deftest declared-type-names-ignores-the-containers
  (testing "a code repository's own `:repositories` entry is not a type about to
            be retired, and reporting it as one would fail every push"
    (is (= {} (pe/declared-type-names {:repositories {:etl {}} :projects {:p {}}})))
    (is (= {} (pe/declared-type-names {:transforms {:parse {}}})))))
