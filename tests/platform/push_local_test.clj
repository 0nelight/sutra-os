(ns ^:unit platform.push-local-test
  (:require [clojure.data]
            [clojure.edn :as edn]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.walk :as walk]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.parser.authoring.standard :as standard]
            [sutra.parser.checkout :as co]
            [sutra.parser.project-export :as pe]
            [sutra.parser.projection :as proj]
            [sutra.types.tree :as tree]
            [support.projection-orakel :as orakel]))

(def ^:private p1 "ri.sutra.main.project.p1")
(def ^:private p2 "ri.sutra.main.project.p2")
(def ^:private p3 "ri.sutra.main.project.p3")

(defn- ot [nm project attrs]
  (cond-> {:object-type-name nm
           :attribute-list   (mapv (fn [a] {:attribute-name (keyword a) :attribute-type :string})
                                   attrs)
           :primary-key      (keyword (first attrs))}
    project (assoc :project project)))

(def ^:private schema
  {:projections  standard/projections
   :projects     {p1 {:project-name :controlling :description "Zahlen"}
                  p2 {:project-name :konstruktion :description "Zeichnungen" :imports [p1]}
                  p3 {:project-name :lager}}
   :object-types {"ri.sutra.main.object-type.o1" (ot :auftrag p1 ["auf-nr" "kunde"])
                  "ri.sutra.main.object-type.o2" (ot :zeichnung p2 ["datei"])
                  "ri.sutra.main.object-type.o3" (ot :teil p3 ["teil-nr" "menge"])
                  "ri.sutra.main.object-type.o4" (ot :notiz nil ["text"])}
   :link-types   {"ri.sutra.main.link-type.l1"
                  {:link-type-name   :auftrag-zeichnung
                   :from-object-type "ri.sutra.main.object-type.o1"
                   :to-object-type   "ri.sutra.main.object-type.o2"
                   :cardinality      :many-to-one
                   :project          p2}
                  "ri.sutra.main.link-type.l2"
                  {:link-type-name   :teil-auftrag
                   :from-object-type "ri.sutra.main.object-type.o3"
                   :to-object-type   "ri.sutra.main.object-type.o1"
                   :cardinality      :many-to-one}}})

(def ^:private nodes
  [[:objects "auftrag"] [:objects "zeichnung"] [:objects "teil"] [:objects "notiz"]
   [:projects "controlling"] [:projects "konstruktion"] [:projects "lager"]
   [:links "auftrag-zeichnung"] [:links "teil-auftrag"]])

(def ^:private paths
  ["ontology.ent" "controlling.objects.ent" "konstruktion.objects.ent" "lager.objects.ent"
   "konstruktion.ent" "controlling.ent" "lager.ent"])

(def ^:private g-memory
  (gen/let [placed (gen/vector (gen/tuple (gen/elements nodes) (gen/elements paths)) 0 4)
            fields (gen/vector (gen/tuple (gen/elements nodes) (gen/elements paths)) 0 3)
            attrs  (gen/vector (gen/tuple (gen/elements [[[:objects "auftrag"] "kunde"]
                                                         [[:objects "teil"] "menge"]])
                                          (gen/elements paths))
                               0 2)]
    (cond-> {}
      (seq placed) (assoc :placement (into {} placed))
      (seq fields) (assoc :fields (into {} (for [[n p] fields] [[n :description] p])))
      (seq attrs)  (assoc :attributes (into {} attrs)))))

(def ^:private g-others
  (gen/elements [nil [:describe "ri.sutra.main.object-type.o1"]
                 [:describe "ri.sutra.main.object-type.o3"]
                 [:create "ri.sutra.main.object-type.o5"]]))

(defn- changed-by-others [s [op k]]
  (case op
    nil       s
    :describe (assoc-in s [:object-types k :description] "von anderen")
    :create   (assoc-in s [:object-types k] (ot :pruefung p1 ["nr"]))))

(def ^:private g-edit
  (gen/tuple (gen/elements [:describe :drop :add :move :drop-text :attr :project :drop-link :noop])
             (gen/choose 0 100) (gen/choose 0 100)
             (gen/elements [:auftrag :zeichnung :teil :notiz :neu :pruefung])))

(defn- body [text] (:schema (edn/read-string text)))
(defn- text [b] (pr-str {:schema b}))

(defn- pick [xs i] (when (seq xs) (nth (vec xs) (mod i (count xs)))))

(defn- edit
  [files [op i j nm]]
  (let [ps   (sort (keep (fn [[p t]] (when t p)) files))
        path (pick ps i)
        b    (some-> path files body)
        obj  (pick (sort (keys (:objects b))) j)]
    (if-not path
      files
      (case op
        :describe  (cond-> files obj (assoc path (text (assoc-in b [:objects obj :description]
                                                                 "meins"))))
        :drop      (cond-> files obj (assoc path (text (update b :objects dissoc obj))))
        :add       (assoc files path
                          (text (assoc-in b [:objects nm]
                                          {:attributes [[:nr :string]] :primary-key :nr})))
        :move      (let [to (pick ps j)]
                     (if (and obj to (not= to path))
                       (-> files
                           (assoc path (text (update b :objects dissoc obj)))
                           (assoc to (text (assoc-in (body (get files to)) [:objects obj]
                                                     (get-in b [:objects obj])))))
                       files))
        :drop-text (assoc files path nil)
        :attr      (cond-> files
                     obj (assoc path (text (update-in b [:objects obj :attributes]
                                                      (fnil conj []) [:extra :string]))))
        :project   (cond-> files
                     obj (assoc path (text (assoc-in b [:objects obj :project] :lager))))
        :drop-link (cond-> files
                     (seq (:links b)) (assoc path (text (update b :links
                                                                #(vec (remove (fn [l]
                                                                                (= l (pick % j)))
                                                                              %))))))
        :noop      files))))

(defn- outcome [f]
  (try {:ok (f)}
       (catch clojure.lang.ExceptionInfo e
         {:refused (or (:refusal/code (ex-data e)) (:kind (ex-data e)) (ex-message e))})))

(defn- said
  [v]
  (walk/postwalk #(if (map? %) (into (empty %) (remove (comp nil? val)) %) %) v))

(defn- world
  [s r]
  (let [live (tree/index (pe/selected-body s {:all-projects? true} pe/ontology-sections))
        put  (tree/index (:schema r))
        gone (set (for [[sec ns] (force (:strict-projected (:opts r))) n ns] [sec n]))]
    (into {} (comp (remove (comp gone key)) (map (fn [[n e]] [n (said (:value e))])))
          (merge live put))))

(defn- source
  [memory]
  (let [els (for [part [:placement :fields :attributes]
                  [el path] (get memory part)]
              [[part el] path (if (= :placement part) el (first el))])]
    (co/source (:files memory)
               (fn [n] (into {} (for [[k path node] els :when (= n node)] [k path])))
               (fn [p] (set (for [[k path] els :when (= p path)] k))))))

(defn- seen [r]
  {:world (world (::schema r) r) :said (:said r) :base (:base r)
   :strict-sections (:strict-sections (:opts r))})

(defspec a-local-push-gives-the-world-of-the-whole-one 1500
  (prop/for-all [memory g-memory
                 others g-others
                 pick-i (gen/vector (gen/choose 0 100) 1 3)
                 edits  (gen/vector g-edit 0 3)]
    (let [rendered (try (proj/render schema :ontologie memory)
                        (catch clojure.lang.ExceptionInfo _ nil))]
      (or (nil? rendered)
          (let [texts (vec (distinct (map #(pick (sort (keys rendered)) %) pick-i)))
                pull  (proj/view schema :ontologie memory {:texts texts})
                files (reduce edit (:files pull) edits)
                now   (changed-by-others schema others)
                opts  {:memory memory :base (:base pull) :shape (:shape pull)}
                run   (fn [absorb]
                        (let [o (outcome #(assoc (absorb now :ontologie files opts) ::schema now))]
                          (if (:ok o) {:ok (seen (:ok o))} o)))
                old   (run orakel/absorb)
                new   (run proj/absorb)
                kept  (run (fn [s nm fs o] (proj/absorb s nm fs (update o :memory source))))]
            (or (= old new kept)
                (do (println "diff" (pr-str (take 2 (clojure.data/diff old new)))
                             "\nkept" (pr-str (take 2 (clojure.data/diff new kept)))
                             "\nedits" (pr-str edits) "\ntexts" texts
                             "\nmemory" (pr-str memory) "\nothers" others)
                    false)))))))
