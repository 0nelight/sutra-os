(ns ^:unit ontology.clojure-edit-function-test
  (:require [clojure.test :refer [deftest is testing]]
            [support.edit-world :as w :refer [handle fails]]
            [sutra.compute.clojure :as cljt]))

(defn- run [source bindings & [extra]] (w/run :clojure source bindings extra))

(defn- updates [log]
  (set (for [[op t p] log :when (= :update-object op)] [t (:id p) (:values p)])))

(deftest a-clojure-function-merges-a-partner-over-its-linked-objects
  (let [[log res] (run "(defn ^{:sutra/function {:edits [:partner :buchung]}}
                          merge-partner [^Partner quelle ^Partner ziel notiz]
                          (doseq [b (ontology/links quelle :buchungen)]
                            (edits/set-link b :partner ziel))
                          (edits/modify ziel {:notiz notiz})
                          (edits/delete quelle))"
                       {:quelle (handle "p1") :ziel (handle "p2") :notiz "zusammengeführt"})]
    (testing "every booking of the source now points at the target (FK link = the key attribute)"
      (is (= #{[:buchung "b1" {:partner-nr "P2"}] [:buchung "b2" {:partner-nr "P2"}]
               [:partner "p2" {:notiz "zusammengeführt"}]}
             (updates log))))
    (is (some #(= [:delete-object :partner {:id "p1"}] %) log))
    (is (some #(and (= :function (:op %)) (= :edit (:lifecycle %))) (:side-effects res)))))

(deftest reads-filter-in-the-platform-vocabulary
  (let [[log] (run "(defn ^{:sutra/function {:edits [:buchung]}} zurueck [^Partner ziel]
                      (doseq [b (ontology/search :buchung
                                                {:where [:in :buchung-nr [\"B2\" \"B3\"]]})]
                        (edits/modify b {:partner-nr (:partner-nr ziel)})))"
                   {:ziel (handle "p1")})]
    (is (= #{[:buchung "b2" {:partner-nr "P1"}] [:buchung "b3" {:partner-nr "P1"}]}
           (updates log)))))

(deftest a-filtered-link-read-starts-at-the-object
  (let [[log] (run "(defn ^{:sutra/function {:edits [:buchung]}} schliessen [^Partner quelle]
                      (doseq [b (ontology/links quelle :buchungen {:where [:= :status \"offen\"]})]
                        (edits/modify b {:status \"zu\"})))"
                   {:quelle (handle "p1")})]
    (is (= #{[:buchung "b1" {:status "zu"}]} (updates log)))))

(deftest create-and-join-table-links
  (let [[log] (run "(defn ^{:sutra/function {:edits [:buchung]}} neu [^Partner partner ^Tag tag]
                      (let [b (edits/create :buchung \"B9\"
                                              {:status \"neu\" :partner-nr (:partner-nr partner)})]
                        (edits/add-link b :tags tag)
                        (edits/remove-link (ontology/get :buchung \"B1\") :tags tag)))"
                   {:partner (handle "p2") :tag (handle "t1")})]
    (is (= [:create-object :create-link :delete-link] (mapv first log)))
    (is (= {:buchung-nr "B9" :status "neu" :partner-nr "P2"} (:values (last (first log)))))))

(deftest a-list-parameter-arrives-once
  (let [[log] (run "(defn ^{:sutra/function {:edits [:partner]}} alle [^\"list[Partner]\" partner]
                      (doseq [p partner] (edits/modify p {:notiz (str (count partner))})))"
                   {:partner [(handle "p1") (handle "p2")]})]
    (is (= #{[:partner "p1" {:notiz "2"}] [:partner "p2" {:notiz "2"}]} (updates log)))))

(deftest a-user-facing-error-reaches-the-user-verbatim
  (is (= "Partner P1 ist gesperrt"
         (fails #(run "(defn ^{:sutra/function {:edits [:partner]}} sperre [^Partner quelle]
                         (sutra/user-facing-error
                          (str \"Partner \" (:partner-nr quelle) \" ist gesperrt\")))"
                      {:quelle (handle "p1")})))))

(deftest an-edit-outside-the-declared-types-is-refused
  (is (re-find #"edits partner, which its .* does not declare"
               (fails #(run "(defn ^{:sutra/function {:edits [:buchung]}} heimlich [^Partner quelle]
                              (edits/modify quelle {:notiz \"x\"}))"
                            {:quelle (handle "p1")})))))

(deftest the-sandbox-still-holds
  (is (fails #(run "(defn ^{:sutra/function {:edits [:partner]}} raus [^Partner quelle]
                     (slurp \"/etc/passwd\"))"
                   {:quelle (handle "p1")}))))

(deftest an-undecorated-defn-is-still-the-dsl
  (let [[log] (run "(defn umhaengen [quelle ziel]
                      (doseq [b (-> quelle (.buchungen))]
                        (-> OntologyClient/action
                            (.update-buchung b {:partner-nr (.partner-nr ziel)}))))"
                   {:quelle (handle "p1") :ziel (handle "p2")}
                   {:param-types {:quelle :partner :ziel :partner}})]
    (is (= 2 (count (updates log))))))

(deftest one-object-per-list-element-each-with-its-own-key
  (testing "the function draws the key (Uuid.random())"
    (let [[log] (run "(defn ^{:sutra/function {:edits [:buchung]}} je-partner
                          [^\"list[Partner]\" ps]
                        (doseq [p ps]
                          (edits/create :buchung (uuid/random)
                                        {:status \"neu\" :partner-nr (:partner-nr p)})))"
                     {:ps [(handle "p1") (handle "p2")]})
          made (for [[op _ p] log :when (= :create-object op)] (:values p))]
      (is (= ["P1" "P2"] (map :partner-nr made)))
      (is (= 2 (count (set (map :buchung-nr made)))) "two creates, two distinct keys")
      (is (every? #(parse-uuid (:buchung-nr %)) made)))))

(deftest a-transform-cannot-draw-a-uuid
  (testing "a transform is a function of its inputs; only an edit mints keys"
    (is (re-find #"Could not resolve symbol: uuid/random"
                 (fails #(cljt/introspect "(fn [_] (uuid/random))"))))))

(def ^:private kern
  {:kern  {:function-name :kern :library true :language :clojure
           :source "(defn notiz-fuer [p] (str \"Kern:\" (:partner-nr p)))"}
   :stamm {:function-name :stamm :library true :language :clojure :uses [:kern]
           :source "(defn notieren [p] (edits/modify p {:notiz (notiz-fuer p)}))"}})

(deftest functions-share-a-library-of-their-repository
  (testing "the library's code is bound before the function — transitively, each once"
    (let [[log] (run "(defn ^{:sutra/function {:edits [:partner]}} merken [^Partner p]
                        (notieren p))"
                     {:p (handle "p1")} {:uses [:stamm :kern] ::w/functions kern})]
      (is (= #{[:partner "p1" {:notiz "Kern:P1"}]} (updates log)))))
  (testing "a library is code for others, never called itself"
    (is (re-find #"is a LIBRARY"
                 (fails #(w/run :clojure "(defn x [])" {} {:library true}))))))

(deftest uses-binds-only-libraries-of-the-same-repository
  (let [msg (fn [extra]
              (fails #(run "(defn ^{:sutra/function {:edits [:partner]}} f [^Partner p] p)"
                           {:p (handle "p1")} extra)))]
    (is (re-find #"names no function" (msg {:uses [:fehlt]})))
    (is (re-find #"another repository"
                 (msg {:uses [:kern] :repository :anderes ::w/functions kern})))
    (is (re-find #"a cycle"
                 (msg {:uses [:kern]
                       ::w/functions (assoc-in kern [:kern :uses] [:stamm])})))))
