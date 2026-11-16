(ns sutra-ui.i18n-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra.i18n.core :as i18n]))

(deftest das-bündel-liegt-im-build
  (i18n/set-locale! :de)
  (is (= "Zeilen" (i18n/t :ui/rows)))
  (i18n/set-locale! :en)
  (is (= "rows" (i18n/t :ui/rows)))
  (testing "ein Platzhalter wird gefüllt"
    (is (= "3 value(s) with leading/trailing space"
           (i18n/t :ui/whitespace-edged {:n 3}))))
  (testing "ein unbekannter Schlüssel bleibt sichtbar"
    (is (= "⟦:ui/gibt-es-nicht⟧" (i18n/t :ui/gibt-es-nicht))))
  (i18n/set-locale! :de))
