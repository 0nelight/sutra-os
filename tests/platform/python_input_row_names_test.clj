(ns ^:unit platform.python-input-row-names-test
  (:require [sutra.types.naming :as naming]
            [clojure.test :refer [deftest is testing]]
            [sutra.actions.effects :as action-effects]
            [sutra.compute.clojure]
            [sutra.compute.external :as ext]
            [sutra.compute.runner :as runner]))

(deftest the-read-key-is-the-one-fold
  (testing "a declared name and the key a transform reads it by"
    (is (= :kategorie_id (naming/read-key :kategorie-id)))
    (is (= :groesse (naming/read-key :grösse)))
    (is (= :name (naming/read-key :name))))
  (testing "only the names the fold changes need an alias"
    (is (= {:kategorie-id :kategorie_id :grösse :groesse}
           (naming/column-aliases [:kategorie-id :grösse :name :schon_gefaltet])))))

(def ^:private in-rows
  [{:id "1" :kategorie_id "k-7" :groesse 3}])

(defn- run-rows
  [rows code manifest-extra]
  (let [path (ext/export-rows-json! rows)]
    (ext/run-transform-local!
     {:transform-name :namen :code code}
     {:input-paths [path]
      :inputs-manifest {:regeln (merge {:kind :dataset :path path} manifest-extra)}})))

(defn- run
  [code manifest-extra]
  (run-rows in-rows code manifest-extra))

(def ^:private read-both
  "from sutra_transforms import transform, Input, Output

@transform(inputs={'regeln': Input(kind='dataset')},
           output=Output(kind='dataset', dataset='aus'))
def lies(regeln):
    r = regeln[0]
    return [{'id': r['id'],
             'deklariert': r.get('kategorie-id'),
             'gespeichert': r['kategorie_id'],
             'umlaut': r['grösse'],
             'enthalten': 'kategorie-id' in r,
             'schluessel': sorted(r.keys()),
             'fehlt': r.get('gibt_es_nicht')}]
")

(deftest a-declared-name-reads-what-was-written
  (let [row (first (:rows (run read-both {:aliases {:kategorie-id :kategorie_id
                                                    :grösse :groesse}})))]
    (testing "the name the transform wrote (and preview shows) reads the value"
      (is (= "k-7" (:deklariert row)))
      (is (= 3 (:umlaut row)))
      (is (true? (:enthalten row))))
    (testing "the stored name still reads — no existing transform breaks"
      (is (= "k-7" (:gespeichert row))))
    (testing "the row is still the stored row: its keys are the stored names"
      (is (= ["groesse" "id" "kategorie_id"] (:schluessel row))))
    (testing "a name with no column anywhere near it keeps dict semantics"
      (is (nil? (:fehlt row))))))

(deftest a-derived-name-is-refused-not-answered-with-none
  (let [code "from sutra_transforms import transform, Input, Output

@transform(inputs={'regeln': Input(kind='dataset')},
           output=Output(kind='dataset', dataset='aus'))
def lies(regeln):
    return [{'id': r['id'], 'k': r.get('summe_netto')} for r in regeln]
"
        res  (try (run-rows (mapv #(assoc % :summe_netto nil) in-rows) code
                            {:derived-names ["summe-netto" "summe_netto"]})
                  (catch Exception e {:error (ex-message e)}))]
    (is (re-find #"derived property" (pr-str res)))))

(deftest a-folded-away-name-is-refused-not-answered-with-none
  (let [code "from sutra_transforms import transform, Input, Output

@transform(inputs={'regeln': Input(kind='dataset')},
           output=Output(kind='dataset', dataset='aus'))
def lies(regeln):
    return [{'id': r['id'], 'k': r.get('kategorie-id')} for r in regeln]
"
        e (try (run code {}) nil (catch Exception e e))]
    (testing "an undeclared column has no alias: the lookup fails LOUDLY"
      (is (some? e))
      (is (re-find #"no column 'kategorie-id'" (str (ex-message e) (pr-str (ex-data e)))))
      (testing "and names the column the transform meant"
        (is (re-find #"meinst du 'kategorie_id'" (str (ex-message e) (pr-str (ex-data e)))))))))

(defn- run-clj
  [code aliases]
  (runner/run-code! :clojure {:transform-name :namen :language :clojure :code code
    :inputs {:regeln {:kind :dataset :dataset :regeln}}
    :output {:kind :dataset :dataset :aus}}
                    {:inputs {:regeln (cond-> {:kind :dataset :rows in-rows}
                                        aliases (assoc :aliases aliases))}}))

(deftest the-same-rule-holds-for-a-clojure-transform
  (let [row (first (:rows (run-clj "(fn [{:keys [regeln]}]
                                      (let [r (first regeln)]
                                        [{:id (:id r)
                                          :deklariert (:kategorie-id r)
                                          :gespeichert (get r :kategorie_id)
                                          :umlaut (r :grösse)
                                          :enthalten (contains? r :kategorie-id)
                                          :schluessel (vec (sort (map name (keys r))))
                                          :fehlt (:gibt-es-nicht r)
                                          :weiter (= (dissoc (assoc r :x 1) :x) (into {} r))}]))"
                                   {:kategorie-id :kategorie_id :grösse :groesse})))]
    (testing "the declared name reads the stored value, via the runner's aliases"
      (is (= "k-7" (:deklariert row)))
      (is (= 3 (:umlaut row)))
      (is (true? (:enthalten row))))
    (testing "the stored name still reads, and the row is still the stored row"
      (is (= "k-7" (:gespeichert row)))
      (is (= ["groesse" "id" "kategorie_id"] (:schluessel row)))
      (is (true? (:weiter row))))
    (testing "a key that folds onto no column keeps map semantics"
      (is (nil? (:fehlt row)))))
  (testing "an undeclared column has no alias: the folded-away key is REFUSED"
    (let [e (try
              (run-clj
               "(fn [{:keys [regeln]}] (mapv (fn [r] {:id (:id r) :k (:kategorie-id r)}) regeln))"
               nil)
              nil
              (catch Exception e e))]
      (is (some? e))
      (is (re-find #"no column :kategorie-id — meinst du :kategorie_id" (str (ex-message e)))))))

(def ^:private declared-rows
  [{:id "1" :kategorie-id "k-7" :grösse 3}])

(def ^:private attrs [:kategorie-id :grösse])

(def ^:private shared-code
  "from sutra_transforms import transform, Input, Output

@transform(inputs={'regeln': Input(kind='dataset')},
           output=Output(kind='dataset', dataset='aus'))
def lies(regeln):
    r = regeln[0]
    return [{'id': r['id'],
             'gefaltet': r['kategorie_id'],
             'deklariert': r.get('kategorie-id'),
             'umlaut': r.get('groesse'),
             'beide': r.get('kategorie_id') or r.get('kategorie-id'),
             'enthalten': 'kategorie_id' in r}]
")

(deftest the-fold-is-inverted-not-copied
  (is (= {:kategorie_id :kategorie-id :groesse :grösse}
         (naming/declared-aliases (conj attrs :name))))
  (is (= (set (vals (naming/column-aliases attrs)))
         (set (keys (naming/declared-aliases attrs))))))

(deftest shared-code-reads-the-same-as-transform-and-as-function
  (let [as-transform (first (:rows (run-rows in-rows shared-code
                                             {:aliases (naming/column-aliases attrs)})))
        as-function  (first (:rows (run-rows declared-rows shared-code
                                             {:aliases (naming/declared-aliases attrs)})))]
    (testing "a Function row keyed by declared names reads the stored fold"
      (is (= "k-7" (:gefaltet as-function)))
      (is (= 3 (:umlaut as-function)))
      (is (true? (:enthalten as-function))))
    (testing "get(folded) or get(declared) no longer trips the refusal"
      (is (= "k-7" (:beide as-function))))
    (testing "both roles give the shared code the same answer"
      (is (= as-transform as-function)))))

(deftest a-function-row-still-refuses-a-real-typo
  (let [code "from sutra_transforms import transform, Input, Output

@transform(inputs={'regeln': Input(kind='dataset')},
           output=Output(kind='dataset', dataset='aus'))
def lies(regeln):
    return [{'id': r['id'], 'k': r.get('kategorie_idx')} for r in regeln]
"
        e (try (run-rows declared-rows code {:aliases (naming/declared-aliases attrs)})
               nil
               (catch Exception e e))]
    (is (some? e))
    (is (re-find #"no column 'kategorie_idx'" (str (ex-message e) (pr-str (ex-data e)))))))

(deftest a-function-object-input-carries-the-declared-aliases
  (let [world {:object-types {:regel {:attribute-list [{:attribute-name :kategorie-id}
                                                       {:attribute-name :grösse}
                                                       {:attribute-name :name}]}}}
        entry (#'action-effects/object-type-input world {:object-type :regel}
                                           [{:id "1" :values {:kategorie-id "k-7"}}])]
    (is (= {:kategorie_id :kategorie-id :groesse :grösse} (:aliases entry)))
    (is (= [{:id "1" :kategorie-id "k-7"}] (:rows entry))))
  (testing "a type whose names the fold leaves alone gets no aliases"
    (is (not (contains? (#'action-effects/object-type-input
                         {:object-types {:t {:attribute-list [{:attribute-name :name}]}}}
                         {:object-type :t} [])
                        :aliases)))))
