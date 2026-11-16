(ns ^:unit platform.platform-experience-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.storage.instance-config :as ic]))

(deftest das-favicon-erbt-nichts
  (is (nil? (:favicon (ic/resolve-logos {:medium "m" :large "l"})))
      "ein Favicon ohne eigenes Bild bleibt leer — sonst zeigt der Tab ein 200px-Logo")
  (is (= "f" (:favicon (ic/resolve-logos {:favicon "f"})))))

(deftest jede-andere-groesse-borgt-sich-eine
  (testing "mittel deckt klein und gross ab"
    (is (= {:favicon nil :small "m" :medium "m" :large "m"}
           (ic/resolve-logos {:medium "m"}))))
  (testing "die Reihenfolge der Kette entscheidet, welche"
    (is (= "m" (:small (ic/resolve-logos {:medium "m" :large "l"}))))
    (is (= "l" (:small (ic/resolve-logos {:large "l"})))))
  (testing "ohne ein einziges Bild bleibt jede Grösse leer"
    (is (= {:favicon nil :small nil :medium nil :large nil}
           (ic/resolve-logos {})))))

(deftest ein-logo-ist-eine-bild-data-uri
  (is (= "" (ic/checked-logo :small "  ")) "leer heisst: diese Grösse räumen")
  (is (= "data:image/png;base64,AAAA" (ic/checked-logo :small "data:image/png;base64,AAAA")))
  (is (thrown? clojure.lang.ExceptionInfo (ic/checked-logo :small "https://example.org/logo.png"))
      "eine fremde URL würde die Kopfzeile jedes Nutzers gegen einen fremden Server ziehen")
  (is (thrown? clojure.lang.ExceptionInfo
               (ic/checked-logo :large
                                (str "data:image/png;base64," (apply str (repeat 1500001 "A")))))))
