(ns sutra-ui.apphost.model-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.model :as model]
            [sutra-ui.state.machine-testing :refer [step]]))

(def ^:private now {:vars {:q "x"} :scenario nil})

(defn- req-of [fx]
  (some (fn [[k a]]
          (case k
            :call     (second (:ok a))
            :dispatch (when (= :rf/request (first a)) (second a))
            nil))
        fx))

(defn- answered [db req answer] (first (step db [:rf/answered req nil answer])))

(deftest nur-die-letzte-antwort-zaehlt
  (let [[db fx1] (step {:door [:tuer] :asked 0} [:capp/fetch {} now])
        [db fx2] (step db [:capp/fetch {:teil? true} now])
        r1       (req-of fx1)
        r2       (req-of fx2)
        db1      (answered db r1 {:a [1]})
        db2      (answered db r2 {:a [2]})]
    (is (= [[:capp/answered {:q "x"}]] (:then r1)))
    (is (= [:tuer {:vars {:q "x"} :held nil :scenario nil}] (:ask r2)))
    (is (= [[:asked] 2] (:current r2)))
    (is (true? (some? (:asking db))))
    (is (nil? (:data db1)) "die Antwort auf die ueberholte Frage faellt weg")
    (is (= {:a [2]} (:data (first (step db2 [:capp/answered {:q "x"} {:a [2]}])))))
    (is (nil? (:asking db2)) "die letzte Antwort beendet das Warten")
    (testing "ein Fehler der letzten Frage beendet nur das Warten"
      (is (nil? (:asking (first (step db [:rf/failed r2 nil "weg"])))))
      (is (nil? (:data (first (step db [:rf/failed r2 nil "weg"]))))))))

(deftest eine-teilantwort-ergaenzt-was-die-seite-haelt
  (let [old {:a [1] :b [2] :_truncated {:a 5 :b 6}}
        d   {:_partial ["a"] :a [3]}]
    (is (= {:a [3] :b [2] :_truncated {:b 6}} (model/with-partial old d)))
    (is (= {:a [3] :b [2] :_truncated {:b 6}}
           (:data (first (step {:data old} [:capp/answered {} d])))))))

(deftest die-naechste-seite-haengt-nur-an-die-antwort-die-sie-fortsetzt
  (let [db {:door [:tuer] :asked 3 :held {:q "x"}
            :data {:a [1 2] :_truncated {:a 5}}}
        [_ fx] (step db [:capp/more :a 2 now])
        r      (req-of fx)]
    (is (= [:tuer {:vars {:q "x"} :scenario nil :more {:a 2}}] (:ask r)))
    (is (= [:capp/more :a 2] (:dedup r)))
    (is (= [[:asked] 3] (:current r)) "eine neuere Antwort der Seite gewinnt")
    (is (= {:a [1 2 3 4] :_truncated {:a 5}}
           (:data (first (step db [:capp/more-rows :a 2 {:a [3 4] :_truncated {:a 5}}])))))
    (is (= {:a [1 2 3 4 5] :_truncated {}}
           (:data (first (step db [:capp/more-rows :a 2 {:a [3 4 5]}]))))
        "die letzte Seite traegt keine Gesamtzahl mehr")
    (is (= db (first (step db [:capp/more-rows :a 1 {:a [9]}])))
        "eine Seite, die nicht anschliesst, aendert nichts")))

(deftest eine-eingabe-fragt-erst-wenn-sie-ruht
  (let [[db fx] (step {:inputs [:q]} [:capp/set-var :root :q nil "x" now])]
    (is (some? (:asking db)) "sofort als unterwegs markiert")
    (is (= [[:after {:key [:input] :ms 250 :event [:capp/fetch {:teil? true} now]}]]
           (remove nil? fx)))
    (is (empty? (remove nil? (second (step {:inputs [:q]} [:capp/set-var :s1 :q nil "x" now]))))
        "eine Variable eines inneren Bereichs ist keine Eingabe der Tuer")))

(deftest ein-modul-gibt-nicht-zurueck-was-es-bekam
  (let [emit :emit
        db   {:module-vars #{:m} :emit emit :handed-in {:m 1}}]
    (is (empty? (remove nil? (second (step db [:capp/set-var :root :m 0 1 now])))))
    (is (= [[:invoke [emit :m 2]]]
           (remove nil? (second (step db [:capp/set-var :root :m 1 2 now])))))
    (testing "neue Werte von aussen setzen die Variablen"
      (let [[db fx] (step db [:capp/module {:m 3} emit {:m :h}])]
        (is (= {:m 3} (:handed-in db)))
        (is (= [[:invoke [reset! :h 3]]] fx))
        (is (empty? (remove nil? (second (step db [:capp/set-var :root :m 1 3 now])))))))))

(deftest ein-szenariowechsel-fragt-den-zweig
  (let [db      {:held {:q "x"}
                 :scopes {:root {:scenario {:var :sc :door [:tuer] :into [:data]}}
                          :s1   {:scenario {:var :sc :door [:tuer] :into [:scopes :s1 :data]}}}}
        [_ fx]  (step db [:capp/set-var :root :sc nil "b" now])
        r       (req-of fx)]
    (is (= [:tuer {:scenario "b"}] (:ask r)))
    (is (= [[:capp/branch-data [:data]]] (:then r)))
    (let [db' (first (step db (conj (first (:then r)) {:a [9]})))]
      (is (= {:a [9]} (:data db')))
      (is (nil? (:held db')) "der Zweig ist nicht mit den Eingaben gefragt"))
    (let [r (req-of (second (step db [:capp/set-var :s1 :sc nil "c" nil])))]
      (is (= [[:capp/branch-data [:scopes :s1 :data]]] (:then r))))))

(deftest das-verlassen-nimmt-die-beobachter-weg
  (let [f   (frame/capp-frame {:inputs [:q] :door [(fn [_] (js/Promise. (fn [_ _])))]})
        q   (atom "a")
        url (atom "u")]
    (frame/send! f [:capp/start {:q q} (constantly now) nil])
    (frame/send! f [:capp/scope :s1 {:var :sc :door [(fn [_])] :into [:x]} {:sc url}])
    (reset! q "b")
    (is (some? (:asking @(:db f))))
    (is (seq (.-watches url)))
    (frame/send! f [:capp/drop-scope :s1])
    (is (empty? (.-watches url)) "ein abgebauter Bereich beobachtet nichts mehr")
    (frame/leave! f)
    (is (empty? (.-watches q))
        "eine URL-Variable haengt am globalen Zustand: frueher fragte die tote App weiter")
    (swap! (:db f) dissoc :asking)
    (reset! q "c")
    (is (nil? (:asking @(:db f))))))
