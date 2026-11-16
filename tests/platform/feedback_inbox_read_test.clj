(ns ^:unit platform.feedback-inbox-read-test
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [sutra.feedback.places :as places]
            [sutra.http.provenance :as provenance]
            [sutra.feedback.http :as fb]
            [sutra.http.security-log :as seclog]
            [sutra.types.feedback :as b]))

(deftest jede-sitzung-traegt-nur-ihren-verlauf
  (let [zeile  (fn [s n] {:sitzung s :ereignis {:id n :art :klick :t n}})
        [a b c] (fb/with-histories [{:id "a"} {:id "b"} {:id "c"}]
                                   [(zeile "a" 1) (zeile "b" 2) (zeile "a" 3)])]
    (is (= 2 (count (:verlauf a))))
    (is (= 1 (count (:verlauf b))))
    (is (empty? (:verlauf c)))))

(deftest ein-fehler-traegt-keinen-getippten-wert
  (let [e (b/fehler {:text "Status: \"geheim\" ist nicht erlaubt\n" :werte ["geheim" "5" ""]
                     :knoten {:aktion "buchen" :param nil}})]
    (is (= "Status: \"…\" ist nicht erlaubt" (:text e)))
    (is (= {:aktion "buchen"} (:knoten e))))
  (is (nil? (b/fehler {:text " \n " :werte []}))))

(deftest der-server-nimmt-einen-fehler-an-und-der-verlauf-nennt-ihn
  (let [roh {:id "1" :t 5 :art "fehler" :ort {:pfad "/x"}
             :text "Pflichtfeld fehlt: Postengruppe" :knoten {:aktion "buchen"}}
        {[e] :angenommen abgelehnt :abgelehnt} (b/stapel [roh])]
    (is (empty? abgelehnt))
    (is (= "Fehler: Pflichtfeld fehlt: Postengruppe · Aktion buchen"
           (:satz (first (b/verlauf [e])))))
    (is (= ["Pflichtfeld fehlt: Postengruppe"] (:fehler (b/zusammenfassung {} [e e]))))))

(deftest die-absage-traegt-den-weg-bis-zum-ursprung
  (let [roh {:id "1" :t 5 :art "fehler" :ort {:pfad "/x"} :text "abgelehnt"
             :selektor ".capp-toast"
             :renderer [{:datei "clients/frontend/src/t.cljs" :zeile 18}]
             :revision "abc123"
             :absage {:methode "POST" :pfad "/api/a/submit" :status 422 :zuordnung "text"
                      :aktion "auftrag-zuordnen" :fehlt ["postengruppe"]
                      :ausloeser {:knoten {:app "a" :pfad "[:pages 0]"} :selektor "button.x"}
                      :browser [{:datei "clients/frontend/src/c.cljs" :zeile 30}]
                      :server [{:datei "libs/kernel/src/sutra/ref/action.clj" :zeile 77
                                :fn "invoke"}]}}
        {[e] :angenommen abgelehnt :abgelehnt} (b/stapel [roh])
        zeilen (:herkunft (first (b/verlauf [e])))]
    (is (empty? abgelehnt))
    (is (= :text (get-in e [:absage :zuordnung])))
    (is (= "Aktion auftrag-zuordnen — fehlt: postengruppe" (first zeilen)))
    (is (some #{"Server libs/kernel/src/sutra/ref/action.clj:77 invoke"} zeilen))
    (is (some #{"Browser clients/frontend/src/c.cljs:30"} zeilen))
    (is (some #{"Stand abc123"} zeilen))))

(deftest ein-stapel-wird-zu-rahmen
  (is (= [{:skript "main.AB.js" :zeile 2 :spalte 345}]
         (b/spur "Error\n at Qk (https://x/js/main.AB.js:2:345)\n at <anonymous>"))))

(deftest die-klassen-eines-selektors-innerste-zuerst
  (is (= ["c__d" "a-b"] (b/klassen "div.a-b > span.c__d:nth-of-type(2)"))))

(deftest die-quellkarte-nennt-zeile-und-quelle
  (let [karte {:sources ["sutra_ui/a.cljs"] :mappings "AAAA;AAEA,KACA"}]
    (is (= {[0 3] {:quelle "sutra_ui/a.cljs" :zeile 0}
            [1 7] {:quelle "sutra_ui/a.cljs" :zeile 3}}
           (places/original karte [[0 3] [1 7]])))
    (is (= {[3 1] {:quelle "sutra_ui/a.cljs" :zeile 2}}
           (places/original {:sections [{:offset {:line 0 :column 0} :map karte}
                                          {:offset {:line 2 :column 0} :map karte}]}
                              [[3 1]]))
        "ein Abschnitt zählt von seinem Anfang an")))

(deftest eine-absage-nennt-ihren-ursprung-nur-auf-nachfrage
  (let [e (doto (ex-info "abgelehnt" {:action :a :missing [:p] :ref/kind :missing-required})
            (.setStackTrace
             (into-array StackTraceElement
                         [(StackTraceElement. "sutra.ref.action$invoke$fn__1" "invoke"
                                              "action.clj" 77)
                          (StackTraceElement. "sutra.serve.server$wrap" "invoke" "server.clj" 5)
                          (StackTraceElement. "clojure.core$apply" "invoke" "core.clj" 9)])))
        res {:status 422 :headers {}}]
    (is (= res (provenance/with-origin {} res e)))
    (is (= res (provenance/with-origin {::seclog/seen (volatile! {})} res e))
        "eine Frage, die keine Anmeldung zugelassen hat, bleibt ohne Antwort")
    (let [req {::seclog/seen (volatile! {:herkunft? true})}
          h   (get-in (provenance/with-origin req res e) [:headers "X-Sutra-Herkunft"])
          k   (json/read-str (java.net.URLDecoder/decode ^String h "UTF-8") :key-fn keyword)]
      (is (= "a" (:aktion k)))
      (is (= ["p"] (:fehlt k)))
      (is (= 1 (count (:server k))) "der Weg durch die Middleware sagt nichts")
      (is (str/ends-with? (:datei (first (:server k))) "sutra/ref/action.clj"))
      (is (= 77 (:zeile (first (:server k)))))
      (is (= "invoke" (:fn (first (:server k))))))))

(deftest ein-fremder-skriptname-fuehrt-nicht-aus-dem-bundle
  (is (= [] (places/browser-frames [{:skript "../../secrets.env" :zeile 1 :spalte 1}]))))
