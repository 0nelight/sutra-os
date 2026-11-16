(ns ^:unit ontology.fk-edge-relation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [sutra.types.world-rules :as rules]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.sql :as sql]))

(def ^:private schema
  {:object-types {:buchungssatz {} :journal-import {}}
   :link-types   {:buchung-import {:from-object-type :buchungssatz
                                   :to-object-type   :journal-import}
                  :tagged         {:from-object-type :buchungssatz
                                   :to-object-type   :journal-import}}
   :transforms
   {:map-journal-imports
    {:transform-name :map-journal-imports
     :output {:kind :object-type :object-type :journal-import :primary-key :import-id}}
    :parse-buchungen
    {:transform-name :parse-buchungen
     :output {:kind :object-type :object-type :buchungssatz :primary-key :buchung-id}}
    :link-buchung-import
    {:transform-name :link-buchung-import
     :input  {:kind :object-type :object-type :buchungssatz}
     :output {:kind :link-type :link-type :buchung-import :fk-attribute :import-id}}
    :link-tagged
    {:transform-name :link-tagged
     :input  {:kind :dataset :dataset :tag_rows}
     :output {:kind :link-type :link-type :tagged}}}})

(defn- relation
  ([W L] (relation W L true))
  ([W L stored?]
   (with-redefs [layout/relation (fn [_c _ns t _as-of]
                                   (when (or stored? (not (str/starts-with? (str t) "c_")))
                                     (sql/frag (str "<" t ">"))))]
     (:sql (layout/link-relation nil "main" (rules/signature W) L nil)))))

(defn- fk-match [W L]
  (#'layout/fk-match (rules/signature W) (get-in (rules/signature W) [:links L :fk])))

(deftest only-fk-backed-links-are-derivable
  (testing "an :endpoint-fk link resolves its two sides and both key columns"
    (is (= {:fk-type :buchungssatz :pk-type :journal-import :fk-side :from
            :fk-attribute :import-id :pk-attribute :import-id}
           (rules/fk-link-spec schema :buchung-import))))

  (testing "a join-table link is NOT derivable — it owns real edge rows"
    (is (nil? (rules/fk-link-spec schema :tagged)))
    (is (nil? (get-in (rules/signature schema) [:links :tagged :fk])))
    (is (= "(SELECT l.\"id\", l.from_id, l.to_id, l.\"_markings\" FROM <c_tagged> l)"
           (relation schema :tagged))
        "it reads its own table and nothing else")))

(deftest the-base-relation-is-the-fk-join
  (let [sql (relation schema :buchung-import false)]
    (testing "both endpoint tables are joined on fk = pk"
      (is (re-find #"FROM <d_buchungssatz> f JOIN <d_journal_import> p ON f\.\S+ = p\." sql))
      (is (re-find #"AS from_id" sql))
      (is (re-find #"AS to_id" sql)))
    (testing "no written links ⇒ no edit layer"
      (is (not (re-find #"(?i)WHERE" sql)))
      (is (not (re-find #"(?i)UNION" sql))))
    (testing "a derived pair has no id of its own and no markings"
      (is (str/starts-with? sql
                            "(SELECT NULL AS \"id\", d.from_id, d.to_id, NULL AS \"_markings\"")))
    (testing "the whole thing is one parenthesised subquery a caller can nest"
      (is (str/starts-with? sql "("))
      (is (str/ends-with? sql ")")))))

(deftest fk-side-decides-which-endpoint-is-from
  (testing ":from — the FK column sits on the from-side"
    (is (re-find #"SELECT f\.\S+ AS from_id, p\.\S+ AS to_id"
                 (relation schema :buchung-import false))))

  (testing ":to — the sides swap, and reading it backwards would still return rows"
    (let [flipped (assoc-in schema [:transforms :link-buchung-import :input :object-type]
                            :journal-import)]
      (is (re-find #"SELECT p\.\S+ AS from_id, f\.\S+ AS to_id"
                   (relation flipped :buchung-import false))
          "which is exactly why the direction is stated rather than assumed"))))

(deftest the-pair-is-unique-without-a-distinct
  (testing "no DISTINCT — a join emits each (f-row, p-row) pair at most once, and
            `internal_uuid` is unique per object table, so the pair is already
            unique. The DISTINCT that used to stand here removed nothing and
            sorted everything: measured on prod 2026-09-21, the biggest FK link
            spilled 45 MB per worker and took 1103 ms against 69 ms without it."
    (is (not (re-find #"(?i)\bDISTINCT\b" (relation schema :buchung-import))))))

(deftest the-join-is-all-there-is
  (testing "no table of written or disconnected links rides on an fk-derived link — a
            row left in one from before hides no pair and adds none"
    (is (not (str/includes? (relation schema :buchung-import) "c_buchung_import")))
    (is (= (relation schema :buchung-import true) (relation schema :buchung-import false)))))

(deftest a-pk-side-without-a-declared-key-matches-leniently
  (let [producerless (-> schema
                         (update :transforms dissoc :map-journal-imports)
                         (assoc-in [:object-types :buchungssatz :attribute-list]
                                   [{:attribute-name :import-id :attribute-type :string}])
                         (assoc-in [:object-types :journal-import :attribute-list]
                                   [{:attribute-name :import-id :attribute-type :string}
                                    {:attribute-name :beleg-nr  :attribute-type :string}
                                    {:attribute-name :tags
                                     :attribute-type [:array :string]}]))]
    (testing "no producer ⇒ no key PROPERTY, so any scalar column may carry the value"
      (is (nil? (:pk-attribute (rules/fk-link-spec producerless :buchung-import))))
      (let [on (fk-match producerless :buchung-import)]
        (is (= "(f.\"import_id\" = p.\"import_id\" OR f.\"import_id\" = p.\"beleg_nr\")" on)
            "the endpoint index resolved against any scalar column; narrowing that
             without a declaration would delete links, not tighten them")
        (is (not (re-find #"tags" on))
            "an array column never carried a scalar FK — the old index skipped it too")))

    (testing "…and only a column of the SAME type: Postgres refuses the comparison outright"
      (let [typed (-> schema
                      (update :transforms dissoc :map-journal-imports)
                      (assoc-in [:object-types :buchungssatz :attribute-list]
                                [{:attribute-name :import-id :attribute-type :string}])
                      (assoc-in [:object-types :journal-import :attribute-list]
                                [{:attribute-name :beleg-nr :attribute-type :string}
                                 {:attribute-name :minuten  :attribute-type :integer}]))
            on (fk-match typed :buchung-import)]
        (is (re-find #"beleg_nr" on))
        (is (not (re-find #"minuten" on)))))

    (testing "a keyless type with NO matching column matches nothing, and says so"
      (let [W (update schema :transforms dissoc :map-journal-imports)]
        (is (= "FALSE" (fk-match W :buchung-import)))
        (is (re-find #"ON FALSE" (relation W :buchung-import false))
            "no column to hold the key against — an honest empty, not a guess")))

    (testing "…while a type WITH a producer is held against that key ALONE"
      (is (= :import-id (:pk-attribute (rules/fk-link-spec schema :buchung-import))))
      (is (= "f.\"import_id\" = p.\"import_id\"" (fk-match schema :buchung-import))
          "this is the end of the collisions the lenient index produced"))))

(deftest an-array-fk-links-every-key-it-holds
  (let [arr (assoc-in schema [:object-types :buchungssatz :attribute-list]
                      [{:attribute-name :import-id :attribute-type [:array :string]}])]
    (testing "one row, a foreign key link to each key in its array"
      (is (= "f.\"import_id\"::jsonb @> jsonb_build_array(p.\"import_id\")"
             (fk-match (assoc-in arr [:object-types :journal-import :attribute-list]
                                 [{:attribute-name :import-id :attribute-type :string}])
                       :buchung-import))))
    (testing "a key of another type is compared as text, as the scalar join does"
      (is (re-find #"jsonb_build_array\(p\.\"import_id\"::text\)"
                   (fk-match (assoc-in arr [:object-types :journal-import :attribute-list]
                                       [{:attribute-name :import-id :attribute-type :integer}])
                             :buchung-import))))))
