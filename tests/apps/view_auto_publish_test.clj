(ns ^:unit apps.view-auto-publish-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.ent :as ent]
            [sutra.parser.project-emit :as pemit]
            [sutra.apps.http.files :as files]
            [sutra.types.versions :as vers]))

(def ^:private body {:kind :rows :items [{:widget :attributes}]})

(deftest the-entv-setting-reaches-every-factor-and-prints-back
  (let [apps (get-in (ent/parse-entv {:object-type :rechnung :auto-publish true
                                      :full body :panel body} "r.entv")
                     [:schema :applications])]
    (is (= #{true} (set (map :auto-publish (vals apps)))))
    (is (true? (:auto-publish (#'pemit/emit-view-file :rechnung (seq apps)))))
    (testing "it is not content: switching it cuts no new version"
      (is (not (contains? (vers/snapshot (:rechnung-view-full apps)) :auto-publish))))))

(def ^:private ot "ri.sutra.main.object-type.11111111-1111-4111-8111-111111111111")
(def ^:private ap "ri.sutra.main.application.22222222-2222-4222-8222-222222222222")

(def ^:private schema
  {:object-types {ot {:id ot :object-type-name :rechnung :plural "Rechnungen"}}
   :applications {ap {:id ap :application-name :rechnung-view-panel :object-type ot
                      :form-factor :panel :status :published :layout body}}})

(deftest saving-one-factor-from-the-builder-switches-the-whole-view
  (let [file  {:name :rechnung-view-full :object-type :rechnung :form-factor :full
               :auto-publish true :layout body}
        saved #(get-in (#'files/parse-view schema % :full "x.entv") [:schema :applications])]
    (is (= {:rechnung-view-full true :rechnung-view-panel true}
           (update-vals (saved file) :auto-publish)))
    (testing "a sibling already agreeing is left alone"
      (is (= [:rechnung-view-full] (keys (saved (dissoc file :auto-publish))))))))
