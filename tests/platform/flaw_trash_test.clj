(ns ^:api platform.flaw-trash-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :once pod/with-mem-pod-auth pod/as-admin)

(defn- install! [schema]
  (let [r (api/post! :install {:schema schema})
        c (when (ok? r) (api/post! :staging))]
    (when-not (and (ok? r) (ok? c)) (api/delete! :staging))
    (and (ok? r) (ok? c))))

(defn- im-trash? [nm]
  (some #(and (= :object-type (keyword (:kind %))) (= nm (:name %)))
        (:trash (:body (api/get! :trash)))))

(deftest trash-lost-inherited
  (is (ok? (api/mutate :create-project {:name :erbe} {:stage? true})))
  (is (ok? (api/post! :staging)))
  (is (install! {:objects {:geerbt {:project :erbe :primary-key :nr :attributes [[:nr :string]]
                                    :plural "Geerbte"}}}))
  (let [b (str "flaw-trash-" (subs (str (random-uuid)) 0 8))]
    (is (ok? (api/post! :branches {:parent "main" :name b})))
    (tx/with-branch b
      (is (not (im-trash? "geerbt")))
      (is (ok? (api/delete-object-type :geerbt)))
      (is (im-trash? "geerbt")
          "the type main declared and the branch deleted is in the branch's trash"))
    (is (not (im-trash? "geerbt")) "main still has it: not in main's trash")))
