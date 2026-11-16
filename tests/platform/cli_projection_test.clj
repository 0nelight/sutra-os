(ns ^:api platform.cli-projection-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.cli.dispatch :as dispatch]
            [sutra.cli.parse :as parse]
            [sutra.cli.projection :as projection]
            [sutra.cli.session :as session]
            [sutra.parser.authoring.standard :as standard]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod)

(defn- tmp []
  (str (java.nio.file.Files/createTempDirectory "cli-projection"
                                                (make-array java.nio.file.attribute.FileAttribute
                                                            0))))

(defn- objects [dir file]
  (set (map name (keys (get-in (edn/read-string (slurp (io/file dir file))) [:schema :objects])))))

(defn- edit! [dir file f]
  (let [x (io/file dir file)]
    (spit x (pr-str (update (edn/read-string (slurp x)) :schema f)))))

(defn- pulled [dir]
  (projection/pull! {} "ontologie" {:to dir})
  (objects dir "handel.objects.ent"))

(deftest a-checkout-is-pulled-edited-and-pushed-without-git
  (is (ok? (api/post! :install
                      {:schema {:projections standard/projections
                                :projects {:werk {} :handel {}}
                                :objects
                                {:kunde
                                 {:primary-key :nr :attributes [[:nr :string]] :match-keys [:nr]
                                  :project :handel
                                  :plural "Kunden"}
                                 :lieferant
                                 {:primary-key :nr :attributes [[:nr :string]] :match-keys [:nr]
                                  :project :handel
                                  :plural "Lieferanten"}
                                 :maschine
                                 {:primary-key :nr :attributes [[:nr :string]] :match-keys [:nr]
                                  :project :werk
                                  :plural "Maschinen"}}}})))
  (is (ok? (api/post! :staging)))
  (let [dir (tmp)]
    (testing "a pull of one text writes that text and a sidecar named after it"
      (is (= 0 (projection/pull! {} "ontologie" {:texts ["handel.objects.ent"] :to dir})))
      (is (= #{"kunde" "lieferant"} (objects dir "handel.objects.ent")))
      (is (not (.exists (io/file dir "werk.objects.ent"))) "no other text")
      (let [side (edn/read-string (slurp (io/file dir ".handel.objects.ent.sutra")))]
        (is (= ["handel.objects.ent"] (:texts side)))
        (is (map? (get-in side [:base "handel.objects.ent"])) "with the base a push merges over")))
    (edit! dir "handel.objects.ent" #(update % :objects dissoc :lieferant))
    (testing "a pull never overwrites an edit it did not see"
      (is (= 1 (projection/pull! {} "ontologie" {:texts ["handel.objects.ent"] :to dir}))))
    (testing "a preview stages nothing"
      (is (= 0 (projection/push! {} [(str dir "/handel.objects.ent")] {:preview? true})))
      (is (= #{"kunde" "lieferant"} (pulled (tmp)))))
    (testing "the push of that one text changes what it holds, and a text it never carried is untouched"
      (is (= 0 (projection/push! {} [(str dir "/handel.objects.ent")] {})))
      (is (ok? (api/post! :staging)))
      (let [d (tmp)]
        (projection/pull! {} "ontologie" {:project "werk" :to d})
        (is (= #{"maschine"} (objects d "werk.objects.ent")))
        (is (.exists (io/file d ".werk.sutra")) "a project's checkout is named after the project"))
      (is (= #{"kunde"} (pulled (tmp)))))
    (testing "a text no pull wrote is never pushed"
      (spit (io/file dir "werk.objects.ent") (pr-str {:schema {:objects {}}}))
      (is (= 1 (projection/push! {} [(str dir "/werk.objects.ent")] {})))
      (is (= #{"maschine"}
             (let [d (tmp)] (projection/pull! {} "ontologie" {:project "werk" :to d})
                  (objects d "werk.objects.ent")))))
    (testing "a renamed text is no deletion: the push refuses, naming the rename"
      (let [d   (tmp)
            old (str d "/handel.objects.ent")]
        (projection/pull! {} "ontologie" {:texts ["handel.objects.ent"] :to d})
        (.renameTo (io/file old) (io/file d "verkauf.objects.ent"))
        (is (= 1 (projection/push! {} [old] {})))
        (is (= #{"kunde"} (pulled (tmp))) "nothing was deleted")
        (testing "and carries it when told to: the node moves to the new text"
          (is (= 0 (projection/push! {} [old] {:adopt? true})))
          (is (ok? (api/post! :staging)))
          (let [all (tmp)]
            (projection/pull! {} "ontologie" {:to all})
            (is (= #{"kunde"} (objects all "verkauf.objects.ent")))
            (is (not (.exists (io/file all "handel.objects.ent")))))
          (is (= ["verkauf.objects.ent"]
                 (:texts (edn/read-string (slurp (io/file d ".handel.objects.ent.sutra")))))
              "the checkout now holds the new text"))))
    (testing "the line-REPL runs projection verbs too: a line is a step of the verb table"
      (is (= {:steps [[:projection-list {}]]}
             (parse/line->cmds (session/new-session {}) "projection list")))
      (is (nil? (dispatch/step! (dispatch/new-ctx {}) [:projection-list {}]))))))

(deftest a-push-over-its-own-staged-push-merges-over-the-pull
  (is (ok? (api/post! :install
                      {:schema {:projections standard/projections
                                :projects {:handel {}}
                                :objects
                                {:kunde
                                 {:primary-key :nr :attributes [[:nr :string]] :match-keys [:nr]
                                  :project :handel
                                  :plural "Kunden"}}}})))
  (is (ok? (api/post! :staging)))
  (let [dir    (tmp)
        text   (str dir "/handel.objects.ent")
        plural! (fn [p] (edit! dir "handel.objects.ent" #(assoc-in % [:objects :kunde :plural] p)))
        now    (fn [] (let [d (tmp)]
                        (projection/pull! {} "ontologie" {:texts ["handel.objects.ent"] :to d})
                        (get-in (edn/read-string (slurp (io/file d "handel.objects.ent")))
                                [:schema :objects :kunde :plural])))]
    (projection/pull! {} "ontologie" {:texts ["handel.objects.ent"] :to dir})
    (plural! "Kundinnen")
    (is (= 0 (projection/push! {} [text] {})))
    (plural! "Kundschaft")
    (is (= 0 (projection/push! {} [text] {}))
        "the second push merges over the pull's base, not over its own staged push")
    (is (ok? (api/post! :staging)))
    (is (= "Kundschaft" (now)))
    (plural! "Klientel")
    (is (= 0 (projection/push! {} [text] {})) "and a push after the commit too")
    (is (ok? (api/post! :staging)))
    (is (= "Klientel" (now)))))

(defn- sent-and-answered
  [text whole?]
  (let [seen (atom nil)
        post api/post!]
    (with-redefs [api/post! (fn [k & args]
                              (let [r (apply post k args)]
                                (when (= :projection-files k)
                                  (reset! seen [(set (keys (:files (last args))))
                                                [(:status r)
                                                 (select-keys (:body r)
                                                              [:created :updated :deleted
                                                               :moved :error])]]))
                                r))]
      (if whole?
        (with-redefs-fn {#'projection/differing (fn [_ files] files)}
          #(projection/push! {} [text] {:preview? true}))
        (projection/push! {} [text] {:preview? true})))
    @seen))

(deftest a-push-sends-the-changed-texts-and-answers-as-the-whole-checkout-would
  (let [kunde (fn [p] {:primary-key :nr :attributes [[:nr :string]] :match-keys [:nr]
                       :project p :plural "Viele"})]
    (is (ok? (api/post! :install
                        {:schema {:projections standard/projections
                                  :projects {:werk {} :handel {} :lager {}}
                                  :objects {:kunde (kunde :handel) :lieferant (kunde :handel)
                                            :maschine (kunde :werk) :regal (kunde :lager)}}})))
    (is (ok? (api/post! :staging)))
    (let [dir  (tmp)
          text (str dir "/handel.objects.ent")
          same (fn [what]
                 (let [[sent answer] (sent-and-answered text false)
                       [all whole]   (sent-and-answered text true)]
                   (is (= answer whole) (str what ": the same answer as the whole checkout"))
                   [sent all]))]
      (projection/pull! {} "ontologie" {:to dir})
      (testing "one changed text of many is all a push sends"
        (edit! dir "handel.objects.ent" #(assoc-in % [:objects :kunde :plural] "Kunden"))
        (let [[sent all] (same "one text")]
          (is (= #{"handel.objects.ent"} sent))
          (is (< 2 (count all)))))
      (testing "a node changed on the server in a text the push does not send stays as it is"
        (let [d (tmp)]
          (projection/pull! {} "ontologie" {:texts ["werk.objects.ent"] :to d})
          (edit! d "werk.objects.ent" #(assoc-in % [:objects :maschine :plural] "Maschinen"))
          (is (= 0 (projection/push! {} [(str d "/werk.objects.ent")] {})))
          (is (ok? (api/post! :staging))))
        (same "another's change")
        (is (= 0 (projection/push! {} [text] {})))
        (is (ok? (api/post! :staging)))
        (let [d (tmp)]
          (projection/pull! {} "ontologie" {:to d})
          (is (= "Maschinen" (get-in (edn/read-string (slurp (io/file d "werk.objects.ent")))
                                     [:schema :objects :maschine :plural])))
          (is (= "Kunden" (get-in (edn/read-string (slurp (io/file d "handel.objects.ent")))
                                  [:schema :objects :kunde :plural])))))
      (testing "a conflict in a sent text is still found"
        (let [d (tmp)]
          (projection/pull! {} "ontologie" {:texts ["handel.objects.ent"] :to d})
          (edit! d "handel.objects.ent" #(assoc-in % [:objects :lieferant :plural] "Theirs"))
          (is (= 0 (projection/push! {} [(str d "/handel.objects.ent")] {})))
          (is (ok? (api/post! :staging))))
        (edit! dir "handel.objects.ent" #(assoc-in % [:objects :lieferant :plural] "Mine"))
        (let [[_ [status]] (sent-and-answered text false)]
          (is (= 409 status))))
      (testing "a node moved between two texts sends both"
        (let [d (tmp)]
          (projection/pull! {} "ontologie" {:to d})
          (let [lager (get-in (edn/read-string (slurp (io/file d "lager.objects.ent")))
                              [:schema :objects :regal])]
            (edit! d "lager.objects.ent" #(update % :objects dissoc :regal))
            (edit! d "werk.objects.ent" #(assoc-in % [:objects :regal] lager)))
          (let [t (str d "/werk.objects.ent")
                [sent answer] (sent-and-answered t false)
                [_ whole]     (sent-and-answered t true)]
            (is (= #{"lager.objects.ent" "werk.objects.ent"} sent))
            (is (= answer whole)))))
      (testing "a deleted text is sent as gone"
        (let [d (tmp)]
          (projection/pull! {} "ontologie" {:to d})
          (io/delete-file (io/file d "lager.objects.ent"))
          (let [t (str d "/werk.objects.ent")
                [sent answer] (sent-and-answered t false)
                [_ whole]     (sent-and-answered t true)]
            (is (= #{"lager.objects.ent"} sent))
            (is (= answer whole))))))))
