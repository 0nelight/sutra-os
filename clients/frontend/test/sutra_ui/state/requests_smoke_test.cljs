(ns sutra-ui.state.requests-smoke-test
  (:require [cljs.test :refer [deftest is testing async]]
            [sutra.kit.remote :as rd]
            [sutra-ui.debug :as debug]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.machine-testing :refer [requests step]]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]))

(defn- f [& _] (js/Promise.resolve nil))

(defn- answered [db req answer] (step db [:rf/answered req nil answer]))

(defn- failed [db req sentence] (step db [:rf/failed req nil sentence]))

(defn- fxs [[_ fx]] (vec (remove nil? fx)))

(deftest a-request-asks-its-call-and-lands-its-answer-at-path
  (let [req     {:ask [f :x 1] :path [:a :b] :pick :rows}
        {:keys [db fx]} (rq/request {} req)]
    (is (= {} db))
    (is (= [[:call {:call [f :x 1] :ok [:rf/answered req nil] :err [:rf/failed req nil]}]] fx))
    (is (= [1 2] (get-in (first (answered {} req {:rows [1 2]})) [:a :b])))
    (is (= [[:rf/request req nil]] (map second [(rq/ask req)])))))

(deftest remote-data-lands-as-rd-values
  (let [req {:ask [f] :path [:d] :rd? true}]
    (is (rd/absent? (:d (first (answered {:d rd/loading} req nil)))))
    (is (= [] (:d (first (answered {:d rd/loading} req [])))))
    (is (= "weg" (rd/reason (:d (first (failed {:d rd/loading} req "weg"))))))
    (is (= [1] (:d (first (failed {:d [1]} req "weg")))) "a value already shown stays"))
  (testing "without :rd? a failure leaves path"
    (is (= [1] (:d (first (failed {:d [1]} {:ask [f] :path [:d]} "weg")))))))

(deftest busy-is-set-while-asked-and-cleared-by-either-end
  (let [req {:ask [f] :busy [:x :busy?]}
        s   (:db (rq/request {} req))]
    (is (true? (get-in s [:x :busy?])))
    (is (false? (get-in (first (answered s req {})) [:x :busy?])))
    (is (false? (get-in (first (failed s req "nein")) [:x :busy?])))
    (is (false? (get-in (first (failed s req nil)) [:x :busy?])))))

(deftest a-failure-puts-its-sentence-at-the-error-path-and-reports-it
  (let [req    {:ask [f] :error [:err] :report "speichern"}
        [s fx] (failed {} req "nein")]
    (is (= "nein" (:err s)))
    (is (= [[:invoke [shell/fail! "speichern" "nein"]]] (fxs [s fx]))))
  (is (= [[:invoke [shell/fail! nil "nein"]]] (fxs (failed {} {:ask [f] :report true} "nein"))))
  (is (= [[:invoke [shell/fail! nil "Fehler: nein"]]]
         (fxs (failed {} {:ask [f] :report #(str "Fehler: " %)} "nein"))))
  (is (= [[:invoke [debug/record! :error "request" "nein"]]] (fxs (failed {} {:ask [f]} "nein")))
      "a failure nothing takes goes to the debug log")
  (is (= [] (fxs (failed {} {:ask [f] :error [:e]} "nein")))))

(deftest then-fx-and-else-follow
  (let [req {:ask [f] :then [[:x/reload 1]] :fx [[:invoke [f]]] :else [[:x/oops]]}]
    (is (= [[:dispatch [:x/reload 1 {:a 1}]] [:invoke [f]] [:invoke [nil {:a 1}]]]
           (second (step {} [:rf/answered req {} {:a 1}]))))
    (is (= [[:dispatch [:x/oops "nein"]]] (fxs (failed {} req "nein"))))))

(deftest a-stale-answer-changes-nothing
  (let [req {:ask [f] :path [:p] :current [[:of] "b1"] :then [[:x/then]] :busy [:busy]}
        [s fx] (answered {:of "b2" :busy true} req {:a 1})]
    (is (nil? (:p s)))
    (is (false? (:busy s)))
    (is (= [] (fxs [s fx])))
    (is (nil? (:p (first (failed {:of "b2"} (assoc req :rd? true) "weg")))))))

(deftest a-dedup-key-asks-once-and-every-reply-settles
  (let [req      {:ask [f] :path [:runs] :dedup [:runs {}]}
        [s1 fx1] (step {} [:rf/request req :r1])
        [s2 fx2] (step s1 [:rf/request req :r2])
        [s3 fx3] (step s2 [:rf/answered req nil [1]])]
    (is (= :call (ffirst fx1)))
    (is (empty? fx2) "a second request of the same key waits for the first")
    (is (= [1] (:runs s3)))
    (is (empty? (::rq/in-flight s3)))
    (is (= 2 (count (fxs [s3 fx3]))))
    (testing "a failure settles them with nil"
      (is (= [[:invoke [nil nil]] [:invoke [nil nil]]]
             (rest (fxs (step s2 [:rf/failed req nil "weg"])))))))
  (is (= [:invoke [:ok 7]] (rq/settle {:resolve :ok} 7))))

(deftest a-request-that-joins-under-dedup-gets-its-own-then-and-else
  (let [r1       {:ask [f] :dedup [:set "1.0"] :then [[:x/eins]] :else [[:x/eins-weg]]}
        r2       (assoc r1 :then [[:x/zwei]] :else [[:x/zwei-weg]])
        [s2 _]   (step (first (step {} [:rf/request r1 nil])) [:rf/request r2 nil])]
    (is (= [[:dispatch [:x/eins {:b 1}]] [:dispatch [:x/zwei {:b 1}]]]
           (filterv #(= :dispatch (first %)) (second (step s2 [:rf/answered r1 nil {:b 1}])))))
    (is (= [[:dispatch [:x/eins-weg "weg"]] [:dispatch [:x/zwei-weg "weg"]]]
           (filterv #(= :dispatch (first %)) (fxs (step s2 [:rf/failed r1 nil "weg"])))))))

(deftest an-aborted-request-leaves-state
  (let [req    {:ask [f] :path [:p] :rd? true :error [:e] :report true :else [[:x/oops]]
                :busy [:b]}
        [s fx] (failed {:p rd/loading :e "alt" :b true} req nil)]
    (is (= {:p rd/loading :e "alt" :b false} s))
    (is (every? nil? fx))))

(deftest a-request-runs-through-a-frame
  (async done
    (let [fr (machine/frame (atom {}) {:name ::t})]
      (machine/reg-event ::go (fn [{:keys [db]} _]
                                (rq/request db {:ask [#(js/Promise.resolve {:n 3})]
                                                :path [:n] :pick :n :busy [:busy]
                                                :then [[:rf/set [:done] true]]})))
      (machine/dispatch [::go] {:frame fr})
      (is (true? (:busy @(:db fr))))
      (js/setTimeout (fn []
                       (is (= {:n 3 :busy false :done true} @(:db fr)))
                       (done))
                     0))))

(deftest a-tagged-busy-holds-its-value-while-asked
  (let [req {:ask [f] :busy [[:x :doing] :save]}
        s   (:db (rq/request {:x {}} req))]
    (is (= :save (get-in s [:x :doing])))
    (is (= {:x {}} (first (answered s req {}))))
    (is (= {:x {}} (first (failed s req nil))))
    (is (= {:x {:doing :other}} (first (answered {:x {:doing :other}} req {})))
        "a later request's tag stays")))

(deftest busy-does-not-reopen-a-closed-container
  (let [req {:ask [f] :busy [:form :busy] :error [:form :err] :within [:form]}]
    (is (= {:form nil} (first (failed {:form nil} req "nein"))))
    (is (= {:form {:busy false :err "nein"}} (first (failed {:form {:busy true}} req "nein"))))
    (is (= {:form nil} (first (answered {:form nil} (assoc req :path [:form :r]) 1))))
    (is (= [[:dispatch [:x/reload 1]]]
           (fxs (answered {:form nil} (assoc req :then [[:x/reload]]) 1)))
        "events still follow")))

(deftest fn-entries-name-their-event-and-invoke-hands-on-x
  (let [req {:ask [f] :then [(fn [a] [:x/saved (:id a)]) (fn [_] nil)]
             :else [(fn [s] [:rf/set [:e] {:error s}]) [:rf/invoke :cb 1]]}]
    (is (= [[:dispatch [:x/saved 3]]] (fxs (answered {} req {:id 3}))))
    (is (= [[:dispatch [:rf/set [:e] {:error "nein"}]] [:dispatch [:rf/invoke :cb 1 "nein"]]]
           (fxs (failed {} req "nein"))))
    (is (= [[:invoke [:cb 1 "nein"]]] (second (step {} [:rf/invoke :cb 1 "nein"]))))))

(deftest always-follows-an-answer-a-failure-and-an-abort
  (let [req {:ask [f] :always [[:x/next]] :error [:e]}]
    (is (= [[:dispatch [:x/next]]] (fxs (answered {} req 1))))
    (is (= [[:dispatch [:x/next]]] (fxs (failed {} req "nein"))))
    (is (= [[:dispatch [:x/next]]] (fxs (failed {} req nil))))))

(deftest a-reply-is-rejected-or-handed-over
  (let [reply {:resolve :ok :reject :no}]
    (is (= :no (-> (step {} [:rf/failed {:ask [f] :reject? true} reply "nein"])
                   second first second first)))
    (is (= [[:invoke [:ok nil]]]
           (second (step {} [:rf/failed {:ask [f] :reject? true} reply nil]))))
    (let [req {:ask [f] :handover? true :then [[:x/on] (fn [a r] [:x/fn a r])]}]
      (is (= [[:dispatch [:x/on 1 reply]] [:dispatch [:x/fn 1 reply]]]
             (second (step {} [:rf/answered req reply 1]))))
      (is (= [[:invoke [:ok nil]]]
             (fxs (step {} [:rf/failed (assoc req :report true) reply nil])))))))

(deftest several-requests-merge-into-one
  (let [a {:ask [f 1] :busy [:a]} b {:ask [f 2] :busy [:b]}
        r (rq/requests {} [a b])]
    (is (= {:a true :b true} (:db r)))
    (is (= [[f 1] [f 2]] (map :call (requests [nil (:fx r)]))))))
