(ns ^:unit platform.push-advisories-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.repos.checks :as repo-checks]
            [sutra.types.advisory :as adv]))

(def ^:private live
  {:applications {:wartung {:layout {:items []}}
                  :faktura {:layout {:items [:alt]}}}})

(def ^:private pushed
  {:applications {:wartung {:layout {:items []}}
                  :faktura {:layout {:items [:neu]}}
                  :neue-app {:layout {:items []}}}})

(deftest ein-push-verantwortet-nur-was-er-aendert
  (is (= #{"faktura" "neue-app"} (repo-checks/authored-apps pushed live)))
  (testing "eine unveraenderte App gehoert nicht dem Push, der sie mitschickt"
    (is (not (contains? (repo-checks/authored-apps pushed live) "wartung")))))

(def ^:private befunde
  [{:severity :warn  :entity "faktura.rechnung-text-setzen" :message "restates the form"}
   {:severity :warn  :entity "wartung.stoerung-melden"      :message "restates the form"}
   {:severity :error :entity "faktura.kaputt"               :message "blocks"}
   {:severity :info  :entity "faktura.kosmetik"             :message "info"}])

(deftest die-warnung-nennt-nur-die-eigenen-apps
  (let [a (repo-checks/advisories-for befunde (repo-checks/authored-apps pushed live))]
    (is (= 1 (count a)))
    (is (re-find #"faktura\.rechnung-text-setzen" (first a)))
    (testing "ein Altfall in einer fremden App bleibt still"
      (is (not (some #(re-find #"wartung" %) a))))
    (testing "Fehler und Infos sind keine Warnungen"
      (is (not (some #(re-find #"blocks|info" %) a))))))

(deftest eine-bestandene-pruefung-kann-trotzdem-sprechen
  (let [detail (repo-checks/with-advisories "schema is structurally valid"
                 ["faktura.x — restates the form"])
        zeilen (repo-checks/side-band-lines {:status :passed
                                        :checks [{:check :lint :status :passed :detail detail}]})]
    (is (= ["sutra: checks passed"
            "sutra: :lint ! faktura.x — restates the form"]
           zeilen))))

(deftest die_warnung_ueberlebt_den_weg_ins_proposal
  (testing "die Check-Zeile, die approve/merge liest, traegt das Detail — sonst kommt dort nie eine Warnung an"
    (let [row (repo-checks/check-row {:check-name "lint" :status "passed"
                                 :detail (repo-checks/with-advisories "gut"
                                           ["faktura.x — restates the form"])})]
      (is (= ["faktura.x — restates the form"] (adv/of-checks [row]))))))
