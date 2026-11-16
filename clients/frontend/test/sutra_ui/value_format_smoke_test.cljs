(ns sutra-ui.value-format-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra.capps.ctx :as cctx]
            [sutra.capps.format :as fmt]))

(def ^:private instant "2020-07-22T13:00:00Z")

(def ^:private de (cctx/of {:locale :de}))

(deftest the-styles-render-as-they-do-on-the-jvm
  (let [t #(fmt/temporal-text instant {:style % :timezone "UTC"} de)]
    (is (= "Mi., 22. Juli 2020" (t :date)))
    (is (= "22.07.2020, 13:00" (t :datetime-short)))
    (is (= "Mi., 22. Juli 2020, 13:00:00" (t :datetime-long)))
    (is (= "13:00" (t :time)))
    (is (= "2020-07-22T13:00:00.000Z" (t :iso-instant)))))

(deftest the-zone-moves-the-wall-clock-here-too
  (is (= "22.07.2020, 15:00"
         (fmt/temporal-text instant {:style :datetime-short :timezone "Europe/Vienna"} de))
      "Intl.DateTimeFormat must reach the same hour java.time does")
  (is (= "22.07.2020, 15:00"
         (fmt/temporal-text instant {:style :datetime-short}
                          (cctx/of {:locale :de :zone "Europe/Vienna"})))
      "and the viewer's zone from the context does the same"))

(deftest a-date-only-value-keeps-its-day
  (is (= "Mi., 22. Juli 2020"
         (fmt/temporal-text "2020-07-22" {:style :date :timezone "Pacific/Kiritimati"} de))))

(deftest the-viewer-locale-decides-words-and-separators
  (let [en (cctx/of {:locale "en-GB"})]
    (testing "the property still says :datetime-short — the viewer says how"
      (is (= "Jul 22, 2020, 1:00 PM"
             (fmt/temporal-text instant {:style :datetime-short :timezone "UTC"} en))))
    (is (= "1,234.50" (fmt/fmt 1234.5 2 en)))
    (is (= "1.2M" ((fmt/by-spec {:notation :compact}) 1234567 en))))
  (is (= "1.234,50" (fmt/fmt 1234.5 2 de)) "and the other viewer is unaffected"))

(deftest an-iso-string-no-longer-reaches-the-page-raw
  (is (= "22.07.2020" (fmt/datum instant de)))
  (is (= "22.07.2020" (fmt/datum "2020-07-22" de))))
