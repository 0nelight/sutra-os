(ns ^:unit platform.identity-providers-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.sso.ldap :as ldap]
            [sutra.access.http.identity :as idp]
            [sutra.types.directory :as directory]
            [sutra.types.settings :as settings]
            [support.mem :as mem]))

(def ^:private fields
  {:enabled {:key "x_enabled" :type :boolean}
   :url     {:key "x_url" :type :string :default "ldap://localhost"}
   :pw      {:key "x_pw" :type :string :secret? true}
   :map     {:key "x_map" :type :edn :default {}}
   :keytab  {:key "x_kt" :type :base64 :input :keytab-b64}})

(deftest settings-are-data
  (testing "decode reads rows by type, defaults what is unset or unreadable"
    (is (= {:enabled true :url "ldap://localhost" :pw "s" :map {} :keytab nil}
           (settings/decode fields {"x_enabled" "true" "x_pw" "s" "x_map" "{"}))))
  (testing "a secret is shown only as whether it is set"
    (is (= {:ready false :enabled true :url "u" :pw? true :map {} :keytab nil}
           (settings/view fields {:enabled true :url "u" :pw "s" :map {}} false))))
  (testing "a change writes what it names; a blank value keeps what is set"
    (is (= {"x_enabled" "false" "x_url" "u2" "x_kt" "QUJD"}
           (settings/rows fields {:enabled false :url "u2" :pw "" :keytab-b64 "QUJD"}))))
  (testing "an unreadable EDN or base64 value is refused, naming the field it came as"
    (is (= :map (:field (settings/problem fields {:map "{"}))))
    (is (= :keytab-b64 (:field (settings/problem fields {:keytab-b64 "!!"}))))
    (is (nil? (settings/problem fields {:map "{:a 1}" :keytab-b64 "QUJD"})))))

(def ^:private fake
  {:label     "Fake"
   :source    "fake"
   :settings  {:enabled {:key "fake_enabled" :type :boolean}}
   :ready?    :enabled
   :directory (fn [_] {:users {"anna@x" {:groups #{"a"}}} :groups {"a" {}}})
   :preview   (fn [_ snap have] {:known (set (keys have)) :listed (set (keys (:users snap)))})})

(deftest the-host-reads-the-directory-a-provider-answers
  (let [store (mem/store)]
    (is (thrown? clojure.lang.ExceptionInfo (idp/preview store fake))
        "not ready: no directory read")
    (idp/set-config! store fake {:enabled true})
    (let [r (idp/preview store fake)]
      (is (= #{"anna@x"} (:listed r)))
      (is (= {"a" {}} (get-in r [:directory :groups])) "the snapshot rides along"))))

(deftest ldap-previews-over-a-snapshot
  (let [cfg  {:realm "X" :group-map {"Einkauf" {:role :ontology-viewer :projects [:portal]}}}
        snap {:users {"anna@x" {:attributes {:sam "anna"} :groups #{"Einkauf"}}
                      "ben@x"  {:attributes {:sam "ben"} :groups #{"Andere"}}}}
        r    (ldap/preview cfg snap {"anna@x" "u-1"})]
    (is (= 1 (:skipped r)) "an unmapped user is skipped")
    (is (= [{:subject "anna@x" :action :exists :user-id "u-1"
             :grants [{:project :portal :role :ontology-viewer}]}]
           (map #(select-keys % [:subject :action :user-id :grants]) (:users r))))))

(deftest a-sync-is-planned-as-data
  (let [users [{:subject "anna@x" :sam "anna" :display "Anna" :action :create
                :grants [{:project :portal :role :ontology-viewer}]}
               {:subject "ben@x" :sam "ben" :action :exists :user-id "u-ben"
                :grants [{:project :lager :role :ontology-editor}]}]
        steps (directory/sync-steps users [:lager] {"old@x" "u-old"})]
    (is (= [{:step :create-project :project :portal}
            {:step :create-user :subject "anna@x" :username "anna" :display "Anna"}
            {:step :grant :user [:subject "anna@x"] :project :portal :role :ontology-viewer}
            {:step :grant :user "u-ben" :project :lager :role :ontology-editor}
            {:step :disable :user "u-old" :subject "old@x"}]
           steps))
    (is (= [] (directory/sync-steps [] [] {})) "nothing to do plans nothing")))

(deftest memberships-follow-the-directorys-own-groups
  (let [snap {:users  {"anna@x" {:groups #{"einkauf" "alle"}} "ben@x" {:groups #{}}}
              :groups {"einkauf" {:groups #{"alle"}} "alle" {}}}
        steps (directory/membership-steps snap {"alle" "g-alle"}
                                          {"anna@x" [:subject "anna@x"] "ben@x" "u-ben"}
                                          #{["u-ben" "einkauf"] ["u-ben" "manuell"]} #{})]
    (is (= [{:step :create-group :group "einkauf"}
            {:step :join :user [:subject "anna@x"] :group "alle"}
            {:step :join :user [:subject "anna@x"] :group "einkauf"}
            {:step :nest :group "einkauf" :in "alle"}
            {:step :leave :user "u-ben" :group "einkauf"}]
           steps)
        "a membership in a group the directory does not name (manuell) is never touched")))
