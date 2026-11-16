(ns sutra-ui.form-machine-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.apphost.action-form]
            [sutra-ui.apphost.form-machine :as fm]
            [sutra-ui.state.machine-testing :refer [step]]))

(defn- seed-of [obj] {:k obj})

(def ^:private specs {:x {} :y {:overrides true}})

(defn- server-params [vals]
  {:x {} :y {:exposure (if (odd? (or (:x vals) 0)) :hidden :editable)}})

(defn- shown [s] (fm/shown-params s specs true))

(defn- rng [n] (let [x (mod (* n 16807) 2147483647)] (if (zero? x) 1 x)))

(defn- pick [w xs]
  (let [r (rng (:r w))] [(assoc w :r r) (nth xs (mod r (count xs)))]))

(declare dispatch)

(defn- effect [w [kind arg]]
  (case kind
    :action-form/schedule     (if (pos? arg)
                                (assoc w :timer true)
                                (dispatch (assoc w :timer false) [:action-form/fire]))
    :action-form/cancel-timer (assoc w :timer false)
    (update w :pending conj [kind arg])))

(defn- dispatch [w event]
  (let [before (:s w)
        [s fx] (step before event)
        moved? (and (not= (shown before) (shown s))
                    (not= :action-form/evaluated (first event))
                    (not (and (= :action-form/seed (first event))
                              (not= (:object-id before) (:object-id (second event))))))
        w      (cond-> (-> w (assoc :s s) (update :trace conj event))
                 moved? (assoc :flicker [event (shown before) (shown s)]))]
    (reduce effect w fx)))

(defn- render [w]
  (let [w (if (= (seed-of (:obj w)) (:seed (:s w)))
            w
            (dispatch w [:action-form/seed {:seed (seed-of (:obj w)) :object-id (:obj w)
                                            :applied (:applied w) :evaluate? true}]))]
    (dispatch w [:action-form/applied (:applied w)])))

(defn- answer [w [kind arg]]
  (case kind
    :action-form/evaluate
    (let [[w ok?] (pick w [true true true false])]
      (dispatch w [:action-form/evaluated arg (when ok? {:params (server-params (:vals arg))
                                                         :values (:vals arg)})]))
    :action-form/baseline
    (dispatch w [:action-form/baselined arg arg])
    :action-form/send
    (let [[w outcome] (pick w [:ok :ok :error :stale])]
      (case outcome
        :ok    (-> w (dispatch [:action-form/submitted (:id arg) {}]) (update :applied inc))
        :error (dispatch w [:action-form/failed (:id arg) "abgelehnt"])
        :stale (dispatch w [:action-form/stale (:id arg)])))))

(defn- deliver [w]
  (let [[w i] (pick w (range (count (:pending w))))
        req   (nth (:pending w) i)
        w     (update w :pending #(into (subvec % 0 i) (subvec % (inc i))))]
    (answer w req)))

(defn- act [w]
  (let [[w a] (pick w (cond-> [:put :put :put :search :submit :switch]
                        (:timer w)          (conj :fire :fire)
                        (seq (:pending w)) (conj :deliver :deliver :deliver)))]
    (case a
      :put     (let [[w p] (pick w [:x :y]) [w v] (pick w (range 4))]
                 (dispatch w [:action-form/put p v specs]))
      :search  (let [[w t] (pick w ["" "a" "ab"])] (dispatch w [:action-form/search :x t]))
      :submit  (dispatch w [:action-form/submit])
      :switch  (update w :obj {"A" "B" "B" "A"})
      :fire    (dispatch (assoc w :timer false) [:action-form/fire])
      :deliver (deliver w))))

(defn- problems
  [{:keys [s obj pending flicker]}]
  (cond-> []
    flicker
    (conj (str "the form redrew without an answer: " (pr-str flicker)))
    (and (:filled s) (not= obj (:k (:filled s))))
    (conj "an answer for another object was drawn")
    (and (:seed s) (not= obj (:k (:base s))))
    (conj "the prefill baseline belongs to another object")
    (not= (boolean (:busy s)) (some? (:submitting s)))
    (conj ":busy disagrees with the submission in flight")
    (< 1 (count (filter (fn [[k a]] (and (= :action-form/send k) (= (:id a) (:submitting s))))
                        pending)))
    (conj "two live submissions at once")))

(defn- quiesce [w]
  (loop [w w n 0]
    (cond
      (< 500 n)          (throw (ex-info "never settled" {}))
      (:timer w)         (recur (render (dispatch (assoc w :timer false) [:action-form/fire]))
                                (inc n))
      (seq (:pending w)) (recur (render (deliver w)) (inc n))
      :else              w)))

(defn- run [seed steps]
  (loop [w (render {:s nil :obj "A" :applied 0 :pending [] :timer false :r seed :trace []})
         i 0]
    (let [bad (problems w)]
      (cond
        (seq bad)    {:seed seed :step i :problems bad :trace (take-last 12 (:trace w))}
        (= i steps)  (let [w (quiesce w)
                           s (:s w)]
                       (when (and (:filled s) (not (:eval-failed s)) (not= (:filled s) (:vals s)))
                         {:seed seed :step :quiet :problems ["a superseded answer won"]
                          :filled (:filled s) :vals (:vals s)
                          :trace (take-last 12 (:trace w))}))
        :else        (recur (render (act w)) (inc i))))))

(deftest answers-in-any-order
  (testing "random interleavings of typing, switching, submitting and late answers"
    (let [fails (keep #(run % 80) (range 1 401))]
      (is (empty? fails) (pr-str (first fails))))))

(deftest a-late-answer-to-an-older-question-is-ignored
  (let [w  (render {:s nil :obj "A" :applied 0 :pending [] :timer false :r 1 :trace []})
        w  (-> w (assoc :pending []) (dispatch [:action-form/put :x 1 {}])
               (dispatch [:action-form/fire]))
        w  (-> w (dispatch [:action-form/put :x 2 {}]) (dispatch [:action-form/fire]))
        [old new] (map second (:pending w))
        w  (-> w (dispatch [:action-form/evaluated new {:values (:vals new)}])
               (dispatch [:action-form/evaluated old {:values (:vals old)}]))]
    (is (= 2 (get-in w [:s :filled :x])))))

(deftest a-second-click-while-submitting-sends-nothing
  (let [[s fx1] (step {:vals {:x 1}} [:action-form/submit])
        [_ fx2] (step s [:action-form/submit])]
    (is (= 1 (count fx1)))
    (is (empty? fx2))))

(deftest a-submission-for-the-previous-object-leaves-the-new-one-alone
  (let [[s]  (step nil [:action-form/seed {:seed {:k "A"} :object-id "A" :applied 0}])
        [s]  (step s [:action-form/submit])
        id   (:submitting s)
        [s]  (step s [:action-form/seed {:seed {:k "B"} :object-id "B" :applied 0}])
        [s]  (step s [:action-form/put :x 7 {}])
        [s'] (step s [:action-form/submitted id {}])]
    (is (identical? s s'))
    (is (= 7 (get-in s' [:vals :x])))))

(deftest a-field-an-override-may-hide-waits-for-the-first-answer
  (let [[s]  (step nil [:action-form/seed {:seed {:x 1} :object-id "A" :applied 0
                                           :evaluate? true}])
        [s'] (step s [:action-form/fire])
        req  {:seq (:eval-seq s') :vals {:x 1}}
        [s2] (step s' [:action-form/evaluated req {:params (server-params {:x 1})
                                                   :values {:x 1}}])]
    (is (= #{:x} (shown s)) "not drawn before the server decided")
    (is (= #{:x} (shown s2)) "and the server hid it: it never appeared")
    (is (= #{:x :y} (fm/shown-params s specs false)) "nothing to wait for without an evaluation")
    (testing "a failed evaluation draws it rather than withholding it for good"
      (let [[s3] (step s' [:action-form/evaluated req nil])]
        (is (= #{:x :y} (shown s3)))))))

(deftest after-a-submission-the-form-keeps-its-shape
  (let [seed  {:seed {:x 0} :object-id "A" :evaluate? true}
        [s]   (step nil [:action-form/seed (assoc seed :applied 0)])
        [s]   (step s [:action-form/fire])
        [s]   (step s [:action-form/evaluated {:seq (:eval-seq s)}
                       {:params (server-params {:x 0}) :values {:x 0}}])
        [s]   (step s [:action-form/submit])
        [s']  (step s [:action-form/submitted (:submitting s) {}])
        [s''] (step s' [:action-form/seed (assoc seed :applied 1)])]
    (is (= #{:x :y} (shown s) (shown s') (shown s'')))
    (is (= {} (:vals s')) "the inputs did start over")))

(deftest the-view-model-is-what-the-form-draws
  (let [specs {:a {:required true :format "0.00"} :b {} :c {:exposure :hidden}}
        s     {:eff    {:a {:required true} :c {:exposure :editable}}
               :opts   {:b {:rows [{:id 1}] :truncated true}}
               :search {:a "x"}
               :vals   {}}
        vm    (fm/view-model s {:specs specs
                                :sections [{:params [:a :b :c]}]
                                :own-options {:a [{:id 9}]}
                                :default-objects {:b [{:id 1} {:id 2}]}})]
    (is (= "0.00" (get-in vm [:specs :a :format])) "an answer keeps the declared format")
    (is (= [:a :b :c] (-> vm :sections first :params)) "the answer revealed :c")
    (is (= #{:a :b} (:truncated vm)) "cut by the server, or narrowed by a term")
    (is (= [{:id 9}] (get-in vm [:options :a])) "a host's own rows come first")
    (is (= [{:id 1} {:id 2}] (get-in vm [:options :b])) "a default joins with its row")
    (is (:blocked? (:gate vm)) "the required :a is missing")
    (is (nil? (:reveal vm))))
  (testing "the first param an override revealed, in form order"
    (let [vm (fm/view-model {:revealed #{:c :b}}
                            {:specs {:a {} :b {} :c {}} :sections [{:params [:a :b :c]}]})]
      (is (= :b (:reveal vm)))))
  (testing "a section with nothing to draw is dropped"
    (let [vm (fm/view-model {} {:specs {:a {:exposure :hidden}}
                                :sections [{:title "T" :params [:a]}]})]
      (is (empty? (:sections vm))))))
