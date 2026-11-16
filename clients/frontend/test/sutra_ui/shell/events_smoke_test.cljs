(ns sutra-ui.shell.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra.kit.remote :as rd]
            [sutra-ui.shell.feedback.recording]
            [sutra-ui.health.watch]
            [sutra-ui.state.loading :as loading]
            [sutra-ui.shell.notifications]
            [sutra-ui.shell.sessions]
            [sutra-ui.resource.versions]
            [sutra-ui.state.machine-testing :refer [requests run step]]))

(defn- kinds [fx] (mapv first (remove nil? fx)))

(defn- failure [status] {::loading/failure {:status status :sentence "nein"}})

(deftest ending-a-session-reloads-and-names-a-timeout
  (let [[s0 :as r] (step {:shared.sessions {}} [:shared.sessions/end {} :others])
        [{:keys [ok]}] (requests r)
        answer (assoc-in (failure 503) [::loading/failure :body] {:data {:incident "i-1"}})
        [_ fx] (step (first (step s0 (conj ok answer))) [:shared.sessions/ended answer])
        s (run s0 (conj ok answer))]
    (is (= :others (get-in s0 [:shared.sessions :busy])))
    (is (nil? (get-in s [:shared.sessions :busy])))
    (is (re-find #"i-1" (get-in s [:shared.sessions :error])))
    (is (= [:dispatch] (kinds fx)) "the list is asked again"))
  (testing "an answer clears the error"
    (is (nil? (get-in (first (step {:shared.sessions {:error "x"}} [:shared.sessions/ended {}]))
                      [:shared.sessions :error])))))

(deftest notifications-count-what-arrives-and-what-is-read
  (let [s {:shared.notifications {:inbox {:items [{:id 1}] :unread 1}}}
        [s' fx] (step s [:shared.notifications/arrived {:id 2}])]
    (is (= [2 1] (map :id (get-in s' [:shared.notifications :inbox :items]))))
    (is (= 2 (get-in s' [:shared.notifications :inbox :unread])))
    (is (= [:invoke] (kinds fx)) "a toast")
    (let [[s''] (step s' [:shared.notifications/marked [2] {:unread 1}])]
      (is (= [true nil] (map :read? (get-in s'' [:shared.notifications :inbox :items]))))
      (is (= 1 (get-in s'' [:shared.notifications :inbox :unread])))))
  (testing "a url closes the list and is followed"
    (let [[s fx] (step {} [:shared.notifications/open false "/x"])]
      (is (false? (get-in s [:shared.notifications :open?])))
      (is (= [:invoke] (kinds fx))))))

(deftest a-version-history-of-an-unsaved-resource-is-none
  (is (= rd/absent (:listing (first (step {:listing rd/loading}
                                          [:shared.versions/listed (failure 404)])))))
  (is (= "nein" (rd/reason (:listing (first (step {:listing rd/loading}
                                                  [:shared.versions/listed (failure 500)]))))))
  (is (= [:a] (:listing (first (step {:listing rd/loading} [:shared.versions/listed [:a]])))))
  (testing "a staged publish clears the note and refreshes what is staged"
    (let [[s [[_ {:keys [ok]}]]] (step {:note "n"} [:shared.versions/stage :x nil {:note ""}])]
      (is (true? (:busy s)))
      (is (= {:busy false :note "" :staged true :error nil} (run s (conj ok {}))))
      (is (= [:dispatch :invoke] (kinds (second (step s (conj ok {})))))))))

(deftest feedback-sends-in-batches-and-ends-the-session
  (let [s {:shared.feedback {:aktiv {:id "f"} :ausgang [{:id "a"} {:id "b"}]}}]
    (is (empty? (second (step (assoc-in s [:shared.feedback :geplant?] true)
                              [:shared.feedback/planen])))
        "one batch is planned at a time")
    (is (= [:call] (kinds (second (step s [:shared.feedback/senden])))))
    (is (= [{:id "b"}] (get-in (first (step s [:shared.feedback/gesendet #{"a"} {}]))
                               [:shared.feedback :ausgang])))
    (is (= [[:dispatch [:shared.feedback/aus]]]
           (second (step s [:shared.feedback/gesendet #{"a"} (failure 409)])))
        "a closed session ends here too")
    (let [[s' fx] (step s [:shared.feedback/beenden [:shared.feedback/starten]
                           [:shared.feedback/fehler]])]
      (is (get-in s' [:shared.feedback :aktiv :beendet?]))
      (is (= [:call] (kinds fx)) "what is still out goes first"))
    (testing "without a session the next one begins at once"
      (is (= [[:dispatch [:shared.feedback/starten]]]
             (second (step {} [:shared.feedback/beenden [:shared.feedback/starten]
                               [:shared.feedback/fehler]])))))))

(deftest a-health-change-reloads-however-it-ended
  (let [[{:keys [ok err]}] (requests (step {} [:shared.health-watch/ask [:f] :reload]))
        dispatched #(filter (comp #{:dispatch} first) (second (step {} %)))]
    (is (= [[:dispatch [:rf/invoke :reload]]]
           (dispatched (conj ok {})) (dispatched (conj err "nein")) (dispatched (conj err nil))))))
