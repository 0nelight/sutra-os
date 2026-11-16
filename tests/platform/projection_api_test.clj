(ns ^:api platform.projection-api-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.parser.authoring.standard :as standard]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod-auth)

(use-fixtures :each pod/as-admin)

(def ^:private schema
  {:schema {:projections (assoc standard/projections
                                :werkstatt
                                  {:filename {:werkstatt {:language :edn :envelope [:schema]
                                                          :name ["werkstatt.ent"] :home-for [:*]}}
                                   :select   {:project :werkstatt :sections [:objects]}})
            :projects    {:buero {} :werkstatt {}}
            :objects     {:auftrag  {:primary-key :auf-nr :attributes [[:auf-nr :string]]
                                     :match-keys [:auf-nr]
                                     :project :buero
                                     :plural "Aufträge"}
                          :maschine {:primary-key :m-nr :attributes [[:m-nr :string]]
                                     :match-keys [:m-nr]
                                     :project :werkstatt
                                     :plural "Maschinen"}
                          :werkzeug {:primary-key :w-nr :attributes [[:w-nr :string]]
                                     :match-keys [:w-nr]
                                     :project :werkstatt
                                     :plural "Werkzeuge"}}}})

(defn- pull [nm]
  (let [r (api/get! :projection-files {:name nm})]
    (is (ok? r) (pr-str (:body r)))
    {:files (update-keys (get-in r [:body :files]) name) :base (get-in r [:body :base])
     :shape (get-in r [:body :shape])}))

(defn- files [nm] (:files (pull nm)))

(defn- write! [nm {:keys [base shape]} fs & {:as opts}]
  (api/post! :projection-files {:name nm} (merge {:files fs :base base :shape shape} opts)))

(defn- objects-in [fs path]
  (set (map name (keys (get-in (edn/read-string (get fs path)) [:schema :objects])))))

(defn- all-objects [fs]
  (into #{} (mapcat #(objects-in fs %)) (keys fs)))

(defn- holding [fs nm]
  (some #(when (contains? (objects-in fs %) nm) %) (sort (keys fs))))

(defn- edit [fs path f]
  (update fs path #(pr-str (update (edn/read-string %) :schema f))))

(defn- installed! []
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging))))

(deftest a-projection-is-read-edited-and-written-back-without-git
  (installed!)
  (testing "the ontology as its standard projection shows it"
    (is (some #{"ontologie"} (map :name (get-in (api/get! :projections) [:body :projections]))))
    (is (= #{"auftrag" "maschine" "werkzeug"} (all-objects (files "ontologie")))))
  (let [{fs :files :as p} (pull "ontologie")
        edited (-> fs
                   (edit (holding fs "werkzeug") #(update % :objects dissoc :werkzeug))
                   (edit (holding fs "auftrag")
                         #(update-in % [:objects :auftrag :attributes] conj [:kunde :string])))]
    (testing "a write without the base of its pull is refused"
      (is (= 400 (:status (write! "ontologie" {} edited)))))
    (testing "a preview stages nothing"
      (let [r (write! "ontologie" p edited :preview? true)]
        (is (ok? r) (pr-str (:body r)))
        (is (get-in r [:body :preview?])))
      (is (= #{"auftrag" "maschine" "werkzeug"} (all-objects (files "ontologie")))))
    (testing "a write stages the change, and the commit lands it"
      (is (ok? (write! "ontologie" p edited)))
      (is (ok? (api/post! :staging)))
      (let [after (files "ontologie")]
        (is (= #{"auftrag" "maschine"} (all-objects after)) "a node the files dropped is deleted")
        (is (str/includes? (get after (holding after "auftrag")) ":kunde")
            "a field they added is there")))))

(deftest a-push-lands-at-once-when-asked-or-where-the-policy-says
  (installed!)
  (testing ":land? true commits in the same request"
    (let [{fs :files :as p} (pull "ontologie")
          r (write! "ontologie" p
                    (edit fs (holding fs "auftrag")
                          #(assoc-in % [:objects :auftrag :description] "sofort"))
                    :land? true)]
      (is (ok? r) (pr-str (:body r)))
      (is (= :ok (keyword (get-in r [:body :landed :status]))))
      (is (empty? (get-in (api/get! :staging) [:body :staged])) "nothing is left staged")
      (is (str/includes? (get (files "ontologie") (holding fs "auftrag")) "sofort"))))
  (testing "a branch whose policy says :land-pushes lands a push unless it asks to stage"
    (is (ok? (api/post! :branch-policy {:name "main"} {:land-pushes true})))
    (try
      (let [{fs :files :as p} (pull "ontologie")
            r (write! "ontologie" p
                      (edit fs (holding fs "auftrag")
                            #(assoc-in % [:objects :auftrag :description] "klein")))]
        (is (get-in r [:body :landed]) (pr-str (:body r))))
      (let [{fs :files :as p} (pull "ontologie")
            r (write! "ontologie" p
                      (edit fs (holding fs "auftrag")
                            #(assoc-in % [:objects :auftrag :description] "gestagt"))
                      :land? false)]
        (is (nil? (get-in r [:body :landed])))
        (is (seq (get-in (api/get! :staging) [:body :staged])) "it waits for a commit")
        (is (ok? (api/post! :staging))))
      (finally (api/post! :branch-policy {:name "main"} {:land-pushes false})))))

(deftest a-stale-folder-does-not-undo-what-changed-since-its-pull
  (installed!)
  (let [{fs :files :as stale} (pull "ontologie")
        other (pull "werkstatt")]
    (is (ok? (write! "werkstatt" other
                     (edit (:files other) "werkstatt.ent"
                           #(assoc-in % [:objects :maschine :description] "von nebenan")))))
    (is (ok? (api/post! :staging)))
    (testing "an edit elsewhere in the stale folder keeps the change it never saw"
      (is (ok? (write! "ontologie" stale
                       (edit fs (holding fs "auftrag")
                             #(assoc-in % [:objects :auftrag :description] "von hier")))))
      (is (ok? (api/post! :staging)))
      (let [after (files "ontologie")]
        (is (str/includes? (get after (holding after "maschine")) "von nebenan"))
        (is (str/includes? (get after (holding after "auftrag")) "von hier"))))
    (testing "the same element changed on both sides is a conflict, named"
      (let [r (write! "ontologie" stale
                      (edit fs (holding fs "maschine")
                            #(assoc-in % [:objects :maschine :description] "von hier")))]
        (is (= 409 (:status r)))
        (is (str/includes? (pr-str (:body r)) "maschine"))))))

(deftest a-push-says-which-inputs-moved-and-what-it-feeds
  (is (ok? (api/post! :install
                      (assoc-in schema [:schema :actions]
                                {:markieren {:params {:nr {:name "Nr" :type :string}}
                                             :project :werkstatt
                                             :rules [{:op :create :object-type :auftrag
                                                      :values {:auf-nr [:get :nr]}}]
                                             :edits [{:object-type :auftrag}]}}))))
  (is (ok? (api/post! :staging)))
  (let [whole  (pull "ontologie")
        fs     (:files whole)
        path   (holding fs "auftrag")
        holder (some (fn [[p t]] (when (str/includes? t ":markieren") p)) fs)
        action (:body (api/get! :projection-files {:name "ontologie" :texts holder}))
        around (:around action)]
    (is (not= path holder) (str "action and type must lie apart here: " holder))
    (is (some #(= "object-type/auftrag" (first %)) (:branch around))
        "the checkout of the action remembers the type it reads")
    (testing "a change says what it feeds, before anything is staged"
      (let [r (write! "ontologie" whole
                      (edit fs path #(update-in % [:objects :auftrag :attributes]
                                                conj [:kunde :string]))
                      :preview? true)]
        (is (ok? r) (pr-str (:body r)))
        (is (some #{"action/markieren"} (get-in r [:body :downstream :direct]))
            (pr-str (:body r)))))
    (testing "a description moves no contract"
      (let [r (write! "ontologie" whole
                      (edit fs path #(assoc-in % [:objects :auftrag :description] "neu"))
                      :land? true)]
        (is (ok? r))
        (is (nil? (get-in r [:body :downstream])) (pr-str (:body r)))
        (is (pos? (get-in r [:body :readers-unaffected])) (pr-str (:body r))))
      (let [r (api/post! :projection-files {:name "ontologie"}
                         {:files {} :base (:base action) :shape (:shape action)
                          :around around :preview? true})]
        (is (= [] (get-in r [:body :inputs-changed])) (pr-str (:body r)))))
    (testing "a new property does, and a push of the stale checkout says so"
      (let [{fs :files :as now} (pull "ontologie")]
        (is (ok? (write! "ontologie" now
                         (edit fs path #(update-in % [:objects :auftrag :attributes]
                                                   conj [:kunde :string]))
                         :land? true))))
      (let [r (api/post! :projection-files {:name "ontologie"}
                         {:files {} :base (:base action) :shape (:shape action)
                          :around around :preview? true})]
        (is (= [{:node "object-type/auftrag" :where "branch" :what "contract"}]
               (get-in r [:body :inputs-changed]))
            (pr-str (:body r)))))))

(deftest a-status-decides-whether-a-text-may-leave-a-type-out
  (installed!)
  (let [set-status (fn [status]
                     (let [{fs :files :as p} (pull "ontologie")]
                       (write! "ontologie" p
                               (edit fs (holding fs "maschine")
                                     #(assoc-in % [:objects :maschine :status] status)))))
        leave-out  (fn []
                     (let [{fs :files :as p} (pull "ontologie")]
                       (write! "ontologie" p
                               (edit fs (holding fs "maschine")
                                     #(update % :objects dissoc :maschine)))))]
    (testing "an active type stays, and the refused push leaves no draft"
      (is (ok? (set-status :active)))
      (is (ok? (api/post! :staging)))
      (let [r (leave-out)]
        (is (not (ok? r)) (pr-str (:body r)))
        (is (re-find #"(?i)active" (pr-str (:body r))) (pr-str (:body r))))
      (is (contains? (all-objects (files "ontologie")) "maschine"))
      (is (empty? (get-in (api/get! :staging) [:body :staged]))))
    (testing "announced first, it may go"
      (is (ok? (set-status :deprecated)))
      (is (ok? (api/post! :staging)))
      (is (ok? (leave-out)))
      (is (ok? (api/post! :staging)))
      (is (not (contains? (all-objects (files "ontologie")) "maschine"))))))

(deftest a-projection-touches-only-what-it-shows
  (installed!)
  (let [{fs :files :as p} (pull "werkstatt")]
    (is (= ["werkstatt.ent"] (keys fs)))
    (is (= #{"maschine" "werkzeug"} (objects-in fs "werkstatt.ent")) "only its project's types")
    (is (ok? (write! "werkstatt" p
                     (edit fs "werkstatt.ent" #(update % :objects dissoc :werkzeug)))))
    (is (ok? (api/post! :staging)))
    (is (= #{"auftrag" "maschine"} (all-objects (files "ontologie")))
        "its own type is gone, another project's type it never showed is untouched")))

(deftest a-projection-refuses-what-it-cannot-say
  (installed!)
  (is (= 400 (:status (write! "werkstatt" (pull "werkstatt") {"ontology.ent" "{:schema {}}"})))
      "a file none of its formats reads")
  (is (= 404 (:status (api/get! :projection-files {:name "gibt-es-nicht"})))))

(deftest a-write-through-the-ontology-moves-a-resource
  (installed!)
  (let [{fs :files :as p} (pull "ontologie")
        from  (holding fs "maschine")
        moved (-> fs
                  (edit from #(update % :objects dissoc :maschine))
                  (assoc "werkstatt.ent"
                         (pr-str {:schema
                                    {:objects
                                       (select-keys
                                        (get-in (edn/read-string (get fs from)) [:schema :objects])
                                        [:maschine])}})))
        r     (write! "ontologie" p moved)]
    (is (ok? r) (pr-str (:body r)))
    (is (some #(= "maschine" (second %)) (get-in r [:body :moved])) "the move is reported")
    (is (empty? (get-in r [:body :staged])) "a move stages nothing: a placement is not the schema")
    (is (= "werkstatt.ent" (holding (files "ontologie") "maschine"))
        "and the next pull shows it where it was put")
    (testing "rehome lays it out at its declared home again"
      (is (= from
             (holding
              (update-keys (get-in (api/get! :projection-files {:name "ontologie" :rehome true})
                                   [:body :files]) name)
              "maschine"))))))
