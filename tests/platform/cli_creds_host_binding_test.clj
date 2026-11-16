(ns ^:unit platform.cli-creds-host-binding-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.cli.auth :as auth]
            [sutra.cli.dispatch :as dispatch]
            [sutra.cli.git]
            [sutra.cli.site :as site]
            [sutra.wire.creds :as creds]
            [sutra.wire.transport :as tx]))

(def ^:private prod "https://ontology.example.com")
(def ^:private solo "http://127.0.0.1:8095")

(defn- tmp-path []
  (let [f (java.io.File/createTempFile "sutra-creds" ".edn")]
    (.delete f)
    (.deleteOnExit f)
    (.getPath f)))

(defmacro ^:private with-base [url & body]
  `(let [prev# (tx/current-base)]
     (tx/set-base! ~url)
     (try (with-redefs [site/prod-url (constantly prod)] ~@body)
          (finally (tx/set-base! prev#)))))

(deftest stored-creds-only-reach-their-own-instance
  (with-redefs [site/prod-url (constantly prod)]
    (let [bound  {:base-url "http://127.0.0.1:8095"
                  :users {:admin {:username "admin" :password "solo-pw" :token "t"}}}
          legacy {:users {:admin {:username "admin" :password "prod-pw" :token "pt"}}}]
      (testing "matching instance: the stored password is used"
        (is (= "solo-pw" (:password (auth/login-credential bound solo nil))))
        (is (nil? (:refused (auth/login-credential bound (str solo "/") nil)))))
      (testing "a legacy file without :base-url belongs to prod"
        (is (= "prod-pw" (:password (auth/login-credential legacy prod nil))))
        (let [r (auth/login-credential legacy solo nil)]
          (is (nil? (:password r)) "the prod password never goes to another host")
          (is (nil? (:token r)) "nor does the cached prod session token")
          (is (= {:von "https://ontology.example.com:443" :nach "http://127.0.0.1:8095"}
                 (:refused r)))))
      (testing "mismatch with SUTRA_ADMIN_PASSWORD: the env password, not the file's"
        (let [r (auth/login-credential legacy solo "env-pw")]
          (is (= "env-pw" (:password r)))
          (is (= "admin" (:username r)))
          (is (nil? (:refused r)))))
      (testing "no creds at all is not a refusal"
        (is (nil? (:refused (auth/login-credential {} solo nil))))))))

(deftest save-creds-binds-the-file-to-the-current-instance
  (let [p (tmp-path)]
    (with-base solo
      (is (auth/save-creds! p {:users {:admin {:username "admin" :password "x"}}}))
      (is (= "http://127.0.0.1:8095" (:base-url (auth/load-creds p)))))
    (testing "a file of another instance is not rebound — its passwords would follow"
      (with-base prod
        (let [err (java.io.StringWriter.)]
          (is (nil? (binding [*err* err]
                      (auth/save-creds! p
                                        (assoc-in (auth/load-creds p) [:users :bob]
                                                  {:password "y"})))))
          (is (str/includes? (str err) "127.0.0.1:8095")))
        (is (= "http://127.0.0.1:8095" (:base-url (edn/read-string (slurp p)))))
        (is (nil? (get-in (auth/load-creds p) [:users :bob])))))))

(deftest credential-helper-carries-a-non-default-creds-file
  (let [helper #'sutra.cli.git/helper-cmd]
    (is (str/ends-with? (helper auth/default-creds-path) " git-credential"))
    (is (str/ends-with? (helper nil) " git-credential"))
    (let [line (helper "solo/creds.edn")]
      (is (str/includes? line " git-credential --creds /"))
      (is (str/ends-with? line (.getAbsolutePath (io/file "solo/creds.edn")))))))

(deftest login-step-passes-the-same-check
  (let [p (tmp-path)]
    (spit p (pr-str {:users {:admin {:username "admin" :password "prod-pw"}}}))
    (with-base solo
      (let [ctx (dispatch/new-ctx {:creds-path p})
            out (let [w (java.io.StringWriter.)]
                  (binding [*out* w *err* w] (dispatch/step! ctx [:login {}]))
                  (str w))]
        (is (nil? @(:token ctx)))
        (is (str/includes? out "https://ontology.example.com:443"))
        (is (str/includes? out "http://127.0.0.1:8095"))))))

(deftest the-one-check-lives-in-core-and-a-legacy-file-binds-only-where-told
  (let [c {:users {:admin {:password "pw"}}}]
    (testing "without a legacy base (the author client knows no prod) a file with no :base-url binds nowhere"
      (is (:refused (creds/bound-user c solo :admin nil))))
    (testing "with one, it binds exactly there"
      (is (= {:password "pw"} (:user (creds/bound-user c prod :admin prod))))
      (is (:refused (creds/bound-user c solo :admin prod))))
    (testing "a bound file answers only its own instance"
      (is (= {:password "pw"} (:user (creds/bound-user (assoc c :base-url solo) solo :admin nil))))
      (is (= {:von "http://127.0.0.1:8095" :nach "https://ontology.example.com:443"}
             (:refused (creds/bound-user (assoc c :base-url solo) prod :admin nil)))))))
