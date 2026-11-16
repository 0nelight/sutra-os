(ns ^:unit platform.ldap-group-map-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.sso.ldap :as ldap]))

(deftest each-group-grants-its-role-on-its-own-projects
  (let [gmap {"Beschaffung"       {:role :ontology-viewer :projects [:einkauf]}
                "Rechnungspruefung" {:role :ontology-editor :projects [:beschaffung]}}
          s    {:group-map gmap}
          u    (ldap/map-user s "edda@example.lan"
                                {:attributes {:sam "edda"}
                                 :groups #{"Beschaffung" "Rechnungspruefung" "Domain Users"}})]
      (testing "the viewer group does not make her a viewer on the editor group's project, nor the reverse"
        (is (= #{{:project :einkauf :role :ontology-viewer}
                 {:project :beschaffung :role :ontology-editor}}
               (set (:grants u)))))
      (testing "an unmapped group grants nothing"
        (is (nil? (ldap/map-user s "x" {:attributes {:sam "x"} :groups #{"Domain Users"}}))))))
