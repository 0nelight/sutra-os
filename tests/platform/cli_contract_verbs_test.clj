(ns ^:unit platform.cli-contract-verbs-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.cli.dispatch :as dispatch]
            [sutra.cli.hints :as hints]
            [sutra.cli.tree :as tree]
            [sutra.parser.step-verbs :as sv]
            [sutra.types.endpoint-verbs :as ev]
            [support.registry :as registry]))

(defn- declared [verbs] (select-keys verbs (keys ev/cli-verbs)))

(deftest every-declared-verb-reaches-tree-and-steps
  (registry/covers "Baum" ev/cli-verbs
                   (filter (set (keys ev/cli-verbs)) (map :verb tree/command-tree)))
  (registry/covers "Schritte" ev/cli-verbs
                   (filter (set (keys ev/cli-verbs)) (map first sv/steps)))
  (registry/complete ev/cli-verbs [:verb :path :section :sum :id :method]))

(deftest the-arguments-follow-the-url-and-the-query
  (testing "die Parameter der URL sind die Positionalen"
    (is (= [:map {:closed true} [:name {:pos 0} [:or :keyword :string]]]
           (ev/cli-args :custom-app-show))
        "ein Name, wie ihn die Zeile als Schluesselwort liefert (stream show)")
    (is (some #{[:custom-app-show :req (ev/cli-args :custom-app-show)]} sv/steps)))
  (testing "ohne Parameter ist das Argument frei"
    (is (some #{[:images :opt [:map {:closed true}]]} sv/steps))))

(deftest the-call-is-the-endpoint
  (is (= {:method :delete :id :proposal :params {:id "7"} :body nil}
         (ev/cli-call :close-proposal {:id "7"})))
  (is (= {:method :post :id :linter-sweep :params {} :body nil}
         (ev/cli-call :linter-sweep {})))
  (testing "ein fester Koerper (:send) kommt mit, auch ohne Feld"
    (is (= {:method :post :id :branch-status :params {:name "b"} :body {:closed true}}
           (ev/cli-call :close {:name "b"})))
    (is (= {:inactive false} (:body (ev/cli-call :reactivate {:name "b"}))))
    (is (= {} (:body (ev/cli-call :approve {:id "7"}))))
    (is (= {:note "ok"} (:body (ev/cli-call :approve {:id "7" :note "ok"})))))
  (testing ":pos legt ein Argument als Positionale in den Koerper"
    (is (= {:method :post :id :egress-policy-decide :params {}
            :body {:id "h:443" :decision :revoke}}
           (ev/cli-call :egress-policy-revoke {:id "h:443"})))
    (is (some #{[:egress-policy-approve :req [:map {:closed true} [:id {:pos 0} :string]]]}
              sv/steps)))
  (testing ":as nennt das Feld, das ein Argument fuellt"
    (with-redefs [ev/cli-verbs {:x {:id :linter-sweep :method :post :as {:why :reason}
                                       :body {:why :string}}}]
      (is (= {:reason "r"} (:body (ev/cli-call :x {:why "r"}))))))
  (testing ":as nennt auch den Parameter der URL, den ein Argument fuellt"
    (is (= {:method :post :id :object-type-drop-edits :params {:type :wa} :body nil}
           (ev/cli-call :drop-edits {:object-type :wa})))
    (is (= [:map {:closed true} [:object-type {:pos 0} [:or :keyword :string]]]
           (ev/cli-args :drop-edits))))
  (testing ":as nennt auch den Parameter der Query, den ein Argument fuellt"
    (is (= {:method :get :id :access-requests :params {:inbox true} :body nil}
           (ev/cli-call :access-requests {:inbox? true})))
    (is (= [:map {:closed true} [:inbox? {:optional true} :boolean]
            [:status {:optional true} :string]]
           (ev/cli-args :access-requests))))
  (testing "ein leerer fester Koerper wird geschickt"
    (is (= {:method :post :id :stream-archive :params {:name :s} :body {}}
           (ev/cli-call :stream-flush {:name :s}))))
  (testing ":resolve fuellt einen Parameter der URL ueber einen Namen"
    (is (= [:map {:closed true} [:name {:pos 0} [:or :keyword :string]]]
           (ev/cli-args :health-check-group-delete)))
    (is (= {:method :delete :id :health-check-group-of :params {} :body nil
            :lookups [{:param :id :from :health-check-groups :rows :groups :by :name
                       :value "nacht"}]}
           (ev/cli-call :health-check-group-delete {:name "nacht"}))))
  (testing ":confirm fragt vor dem Aufruf"
    (is (string? (:confirm (ev/cli-verbs :drop-branch))))))

(deftest a-declared-verb-has-no-step-method-of-its-own
  (dispatch/load-verbs!)
  (is (empty? (declared (methods dispatch/step!)))))

(deftest a-declared-verb-hint-keeps-the-hint-vocabulary
  (testing ":cli :section ist der Abschnitt im Baum, nie das :theme eines Hinweises"
    (is (nil? (:theme (hints/verb-hint :password))))
    (is (= :operate (:theme (hints/verb-hint :images)))))
  (testing "ein Hinweis ergaenzt die :sum des Vertrags, statt sie zu ersetzen"
    (is (every? (comp :purpose hints/verb-hint) (keys ev/cli-verbs)))))
