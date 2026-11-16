(ns sutra-ui.apphost.app-watch-smoke-test
  (:require [cljs.test :refer-macros [deftest is use-fixtures]]
            [sutra-ui.state.portal :as pstate]
            [sutra-ui.apphost.client :as sdk-client]))

(def ^:private aufmachen #'pstate/watch-consumer-app!)
(def ^:private zumachen  #'pstate/stop-consumer-app-watch!)

(def ^:private hoerer (atom {}))

(use-fixtures :each
  {:before (fn []
             (reset! hoerer {})
             (set! js/document
                   #js {:hidden false
                        :addEventListener (fn [typ f] (swap! hoerer assoc typ f))
                        :removeEventListener (fn [typ _] (swap! hoerer dissoc typ))}))})

(defn- falscher-socket
  [protokoll]
  (fn [url]
    (swap! protokoll update :offen (fnil conj []) url)
    (let [ws #js {:close (fn [] (swap! protokoll update :zu (fnil inc 0)))
                  :send  (fn [text] (swap! protokoll update :gesendet (fnil conj [])
                                           (js->clj (js/JSON.parse text) :keywordize-keys true)))}]
      (swap! protokoll assoc :socket ws)
      ws)))

(defn- verbinden!
  [protokoll]
  ((.-onopen (:socket @protokoll)) #js {}))

(defn- abonnierte-typen
  [protokoll]
  (set (map (comp :objectType :objectSet) (:requests (first (:gesendet @protokoll))))))

(deftest ein-socket-fuer-alle-gebundenen-typen
  (let [protokoll (atom {})]
    (with-redefs [sdk-client/socket-oeffnen (falscher-socket protokoll)]
      (aufmachen {:id "app-1"} {:object-names {:r1 "angebot" :r2 "anfrage-verlauf"}})
      (is (= 1 (count (:offen @protokoll)))
          "EIN Socket — ein Strom je Typ sprengte das 6-Verbindungen-Budget des Browsers")
      (zumachen)
      (is (= 1 (:zu @protokoll))))))

(deftest abonniert-wird-der-name-nicht-die-rid
  (let [protokoll (atom {})]
    (with-redefs [sdk-client/socket-oeffnen (falscher-socket protokoll)]
      (aufmachen {:id "app-2"}
                 {:object-names
                  {:ri.sutra.main.object-type.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee "angebot"}})
      (verbinden! protokoll)
      (is (= #{"angebot"} (abonnierte-typen protokoll))
          "die Tabellen sind nach RID gekeyed — die Watch-Tuer loest nur den Namen auf")
      (zumachen))))

(deftest ohne-gebundene-typen-wird-nichts-aufgemacht
  (let [protokoll (atom {})]
    (with-redefs [sdk-client/socket-oeffnen (falscher-socket protokoll)]
      (aufmachen {:id "app-3"} {:object-names {}})
      (is (nil? (:offen @protokoll)))
      (zumachen))))

(deftest ein-zweites-oeffnen-loest-das-erste-ab
  (let [protokoll (atom {})]
    (with-redefs [sdk-client/socket-oeffnen (falscher-socket protokoll)]
      (aufmachen {:id "app-4"} {:object-names {:r1 "angebot"}})
      (aufmachen {:id "app-4"} {:object-names {:r1 "angebot"}})
      (is (= 1 (:zu @protokoll))
          "sonst sammelt ein Portal bei jedem Wechsel eine Leiche mehr an")
      (zumachen))))

(deftest eine-pause-haelt-die-meldung-zurueck-und-holt-sie-beim-einschalten-nach
  (let [protokoll (atom {})
        gerufen   (atom 0)]
    (with-redefs [sdk-client/socket-oeffnen (falscher-socket protokoll)]
      (let [stop (sdk-client/watch! nil {:types ["anruf"] :min-seconds 0
                                         :on-change (fn [_] (swap! gerufen inc))})]
        (sdk-client/auto-refresh! false)
        ((.-onmessage (:socket @protokoll))
         #js {:data (js/JSON.stringify #js {:type "refreshObjectSet"})})
        (is (= 0 @gerufen) "apphost/auto-refresh.md: pausiert wirkt keine Meldung")
        (sdk-client/auto-refresh! true)
        (is (= 1 @gerufen) "eingeschaltet wird sie nachgeholt")
        (stop)))))

(deftest nach-einem-verbindungsabbruch-wird-einmal-nachgeladen
  (let [protokoll (atom {})
        gerufen   (atom 0)]
    (with-redefs [sdk-client/socket-oeffnen (falscher-socket protokoll)]
      (let [stop (sdk-client/watch! nil {:types ["anruf"] :min-seconds 0
                                         :on-change (fn [_] (swap! gerufen inc))})
            erster (:socket @protokoll)]
        ((.-onopen erster) #js {})
        (is (= 0 @gerufen) "die erste Verbindung ist kein Nachladen")
        ((.-onclose erster) #js {})
        ((.-onopen erster) #js {})
        (is (= 1 @gerufen) "was im Abbruch geschah, hat niemand gemeldet")
        (stop)))))

(deftest ein-verdeckter-tab-holt-die-meldung-beim-sichtbarwerden-nach
  (let [protokoll (atom {})
        gerufen   (atom 0)]
    (with-redefs [sdk-client/socket-oeffnen (falscher-socket protokoll)]
      (let [stop (sdk-client/watch! nil {:types ["anruf"] :min-seconds 0
                                         :on-change (fn [_] (swap! gerufen inc))})
            melden #((.-onmessage (:socket @protokoll))
                     #js {:data (js/JSON.stringify #js {:type "refreshObjectSet"})})]
        (set! (.-hidden js/document) true)
        (melden)
        (is (= 0 @gerufen) "apphost/auto-refresh.md: solange verdeckt, wartet die Meldung")
        (set! (.-hidden js/document) false)
        ((get @hoerer "visibilitychange") nil)
        (is (= 1 @gerufen) "beim Sichtbarwerden laedt sie sofort")
        ((get @hoerer "visibilitychange") nil)
        (is (= 1 @gerufen) "und nur einmal")
        (stop)
        (is (nil? (get @hoerer "visibilitychange")) "stop nimmt den Hoerer wieder weg")))))

(deftest eine-menge-ohne-sichtbares-widget-wartet-bis-eines-sie-nutzt
  (let [protokoll (atom {})
        gerufen   (atom 0)
        menge     {:type "base" :objectType "anruf"}
        verdeckt  (atom #{menge})]
    (with-redefs [sdk-client/socket-oeffnen (falscher-socket protokoll)]
      (let [stop   (sdk-client/watch! nil {:object-sets [menge] :min-seconds 0
                                           :verdeckt verdeckt
                                           :on-change (fn [_] (swap! gerufen inc))})
            melden #((.-onmessage (:socket @protokoll))
                     #js {:data (js/JSON.stringify (clj->js %))})]
        (melden {:type "subscribeResponses" :responses [{:type "success" :id "s1"}]})
        (melden {:type "objectSetChanged" :id "s1"})
        (is (= 0 @gerufen) "apphost/auto-refresh.md: verdeckt wirkt keine Meldung")
        (reset! verdeckt #{})
        (is (= 1 @gerufen) "sobald sie sichtbar ist, laedt sie sofort")
        (melden {:type "objectSetChanged" :id "s1"})
        (is (= 2 @gerufen) "und danach wie jede andere")
        (stop)))))
