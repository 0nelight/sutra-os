(ns ^:unit grammar.workspace-shape-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.parser.resolve :as res]
            [sutra.types.rid :as rid]
            [sutra.authoring.install.plan :as install-plan]))

(deftest ein-home-eintrag-kennt-die-lebenden-apps
  (let [ws {:workspaces {:gf {:workspace-name :gf
                              :apps [:leitwarte] :landing :leitwarte
                              :home [{:column :a
                                      :items [{:kind :app :app :leitwarte}]}]}}}
        msgs #(mapv :message (res/findings ws %))]
    (testing "die App lebt auf der Instanz, steht aber in keiner .ent dieses Pushes"
      (is (= [] (msgs {:application-names #{:leitwarte}}))))
    (testing "eine wirklich unbekannte App wird weiterhin abgewiesen"
      (is (= 1 (count (msgs {:application-names #{:flottenmodul}})))))
    (testing "die Live-Tafel des Push-Pfades führt :application-names"
      (is (contains? (set (keys (install-plan/live-ref-symbols
                                 {:applications {} :object-types {}
                                  :link-types {} :interfaces {}
                                  :functions {} :transforms {}
                                  :actions {} :object-sets {}
                                  :llm-models {} :projects {}})))
                     :application-names)))))

(deftest ein-home-eintrag-wird-auf-eine-rid-umgeschrieben
  (let [ws  {:workspace-name :gf
             :apps [:leitwarte] :landing :leitwarte
             :home [{:column :a :items [{:kind :app :app :leitwarte}
                                        {:kind :object-type :object-type :arbeitsauftrag}
                                        {:kind :link :url "https://x/y"}]}]}
        rid (fn [_targets v] (get {:leitwarte :ri.app.main.x
                                  :arbeitsauftrag   :ri.ot.main.y} v v))
        out (rid/rewrite-refs :workspaces ws rid)
        its (get-in out [:home 0 :items])]
    (testing ":apps und :landing wie bisher"
      (is (= [:ri.app.main.x] (:apps out)))
      (is (= :ri.app.main.x (:landing out))))
    (testing "der Home-Eintrag zeigt auf dieselbe RID wie :apps"
      (is (= :ri.app.main.x (:app (first its)))))
    (testing "ein Objekttyp-Eintrag ebenso"
      (is (= :ri.ot.main.y (:object-type (second its)))))
    (testing "ein Link bleibt unangetastet"
      (is (= "https://x/y" (:url (nth its 2)))))))
