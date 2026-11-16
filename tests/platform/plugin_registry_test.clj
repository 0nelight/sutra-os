(ns ^:unit platform.plugin-registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.kit.system :as sys]))

(defn- refusal [m]
  (try (sys/check-module m) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(deftest a-module-is-checked-before-it-is-folded
  (testing "a well-formed module passes"
    (is (nil? (refusal {:name :ok :provides {:watch/wake (fn [])}
                        :contributes {:http/routes {:r {:get identity}}}
                        :jobs [{:name :j :start (fn [_]) :every "X_MS" :default 1000}]}))))
  (testing "form: a misspelt key, a job without :start"
    (is (re-find #"malformed" (refusal {:name :x :provide {}})))
    (is (re-find #"malformed" (refusal {:name :x :jobs [{:stop identity}]}))))
  (testing "a capability or table nobody registered is refused"
    (is (re-find #"not a registered capability" (refusal {:name :x :provides {:no/such 1}})))
    (is (re-find #"not a registered capability" (refusal {:name :x :wants [:no/such]})))
    (is (re-find #"not a registered table" (refusal {:name :x :contributes {:no/table {:a 1}}}))))
  (testing "a provided value or a contribution in the wrong shape is refused"
    (is (re-find #"wrong shape" (refusal {:name :x :provides {:store/db 3}})))
    (is (re-find #"wrong shape" (refusal {:name :x :provides {:auth/app-guard {:guard identity}}})))
    (is (re-find #"wrong shape" (refusal {:name :x :contributes {:http/routes {:r {}}}})))
    (is (re-find #"wrong shape"
                 (refusal {:name :x :contributes {:http/routes {:r {:gett identity}}}})))
    (is (re-find #"wrong shape"
                 (refusal {:name :x :contributes {:http/prefix-authority {:not-a-path {}}}})))))

(deftest instrumented-capabilities-check-their-arity
  (let [s (sys/instrument (sys/assemble [{:name :x :provides {:schema/array-cols (fn [_ _] [])
                                                              :watch/wake (fn [] :woken)}}]))]
    (is (= [] ((get-in s [:provides :schema/array-cols]) {} "t")))
    (is (= :woken ((get-in s [:provides :watch/wake]))))
    (is (thrown? Exception ((get-in s [:provides :schema/array-cols]) {})))))

(deftest a-role-plugin-contributes-only-to-role-tables
  (is (nil? (sys/role-problem {:name :r :contributes {:identity/providers {}}})))
  (is (re-find #"no role"
               (sys/role-problem {:name :r :contributes {:http/routes {:x {:get identity}}}})))
  (is (re-find #"provides" (sys/role-problem {:name :r :provides {:watch/wake (fn [])}})))
  (is (re-find #"jobs" (sys/role-problem {:name :r :jobs [{:start (fn [])}]})))
  (testing "every role plugin on the classpath is one"
    (is (every? (comp nil? sys/role-problem)
                (filter (comp #{:auth-ldap :auth-spnego :llm-xai :packages} :name)
                        (sys/discover))))))

(deftest a-tick-job-is-declared-and-run-by-the-ticker
  (let [job {:name :t :every "T_MS" :default 1000 :tick (fn [_])}]
    (testing "a module only declares it: no :start, a :name and an :every"
      (is (nil? (refusal {:name :x :jobs [job]})))
      (is (re-find #"malformed" (refusal {:name :x :jobs [(dissoc job :every)]}))))
    (testing "it needs a module providing :jobs/tick"
      (is (re-find #"jobs/tick" (try (sys/assemble [{:name :x :jobs [job]}]) nil
                                     (catch clojure.lang.ExceptionInfo e (ex-message e))))))
    (testing "start-jobs! and stop! hand it to the ticker"
      (let [seen   (atom [])
            ticker {:name :ticker
                    :provides
                    {:jobs/tick {:start (fn [j ms] (swap! seen conj [:start (:name j) ms]))
                                 :stop  (fn [j] (swap! seen conj [:stop (:name j)]))}}}]
        (let [s (sys/install! (sys/assemble [ticker {:name :x :jobs [job]}]))]
          (try
            (sys/start-jobs! s {"T_MS" "250"})
            (finally (sys/stop! s))))
        (is (= [[:start :t 250] [:stop :t]] @seen))))))

(deftest opened-capabilities-are-built-by-configure-in-dependency-order
  (let [closed (atom 0)
        pools  (reify
                 clojure.lang.IFn (invoke [_ lane] [:ds lane])
                 java.lang.AutoCloseable (close [_] (swap! closed inc)))
        s      (sys/assemble
                [{:name :c :requires [:pg/ds]
                  :opens {:coord/db (fn [_ {p :pg/ds}] (fn [] (p nil)))}}
                 {:name :p :opens {:pg/ds (fn [config _] (when (= "u" (get config "U")) pools))}}])]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"configure it"
                          (sys/install! s))
        "a system that opens capabilities is not installed before they are opened")
    (let [c (sys/configure s {"U" "u"})]
      (is (= [:ds nil] ((get-in c [:provides :coord/db])))
          ":coord/db got the :pg/ds that was opened before it")
      (sys/close! c)
      (is (= 1 @closed) "close! closes what is AutoCloseable"))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"contributed by both"
                          (sys/assemble [{:name :a :provides {:coord/db (fn [])}}
                                         {:name :b :opens {:coord/db (fn [_ _] (fn []))}}])))))
